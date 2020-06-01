(ns imo.analysis
  (:require [imo.analysis.spec :as s]
            [imo.analysis.built-in :as built-in]
            [imo.logger :refer [timed warn]]
            [imo.util :refer [node? start-of end-of node->source simple-symbol-str?]])
  (:import (imo AnalysisException)
           (clojure.lang Symbol Keyword)
           (java.util Map)
           (imo.analysis.spec Acc Spec)))

(defrecord Binding
  [^Symbol local-name
   ^Symbol fq-name])

(defrecord Alias
  [^Symbol local-name
   ^Symbol target-ns])

(defrecord Scope [parent ^Map bindings])

(defrecord Context
  [^Scope scope
   ^Map aliases
   ^Map sym-resolution
   ^Map ns-exports
   ^String current-ns
   ^Keyword mode
   ^Binding recur-target])

(defn ctx?
  "Returns boolean whether the given value is context or not"
  [x]
  (instance? Context x))

(defn create-binding
  "Creates new binding with the given fully qualified and local name"
  [local-name fq-name]
  {:pre [(simple-symbol? local-name)
         (symbol? fq-name)]}
  (->Binding local-name fq-name))

(defn create-alias
  "Creates alias pointing to the given namespace"
  [local-name target-ns]
  {:pre [(simple-symbol? local-name)
         (simple-symbol? target-ns)]}
  (->Alias local-name target-ns))

(defn add-alias
  "Adds alias to the context and returns the updated context"
  [{:keys [aliases] :as ctx} alias]
  {:pre [(instance? Alias alias)
         (map? aliases)]}
  (assoc-in ctx [:aliases (name (:local-name alias))] alias))

(defn add-binding
  "Adds binding to the context's current lexical scope and returns
   the updated context"
  [{:keys [scope] :as ctx} b]
  {:pre  [(instance? Scope scope)
          (instance? Binding b)]
   :post [(ctx? %)]}
  (assoc-in ctx [:scope :bindings (name (:local-name b))] b))

(defn push-lexical-scope
  "Pushes new lexical scope the the context"
  [ctx]
  {:pre  [(ctx? ctx)]
   :post [(ctx? %)]}
  (assoc ctx :scope (->Scope (:scope ctx) {})))

(defn pop-lexical-scope
  "Pops the latest lexical scope and all bindings added on it"
  [ctx]
  {:pre  [(ctx? ctx)]
   :post [(ctx? %)]}
  (update ctx :scope :parent))

(defn set-mode
  "Sets new mode to the context and returns the updated context"
  [ctx mode]
  {:pre  [(contains? #{:eval :quote :syntax-quote} mode)
          (ctx? ctx)]
   :post [(ctx? %)]}
  (assoc ctx :mode mode))

(defn set-recur-target
  "Sets new recur target to the context and returns the
   updated context"
  [ctx target]
  {:pre  [(or (instance? Binding target)
              (nil? target))]
   :post [(ctx? %)]}
  (assoc ctx :recur-target target))

(defn resolve-binding
  "Resolves binding based on it's local name"
  [{:keys [scope] :as ctx} local-name]
  {:pre [(ctx? ctx)
         (simple-symbol? local-name)]}
  (loop [{:keys [bindings parent]} scope]
    (let [b (get bindings (name local-name))]
      (cond (some? b) b
            (some? parent) (recur parent)
            :else nil))))

(defn resolve-alias
  "Resolves alias for the given name"
  [{:keys [aliases] :as ctx} alias]
  {:pre [(ctx? ctx)
         (string? alias)]}
  (get aliases alias))

(defn resolve-fq-name
  "Resolves fully qualified name for the given local name
   and returns a tuple of `[fq-name source]` where source
   can be either alias or binding or `nil` if fully qualified
   name can't be resolved"
  [ctx local-name]
  {:pre [(ctx? ctx)
         (symbol? local-name)]}
  (if-let [ns (namespace local-name)]
    (if-let [alias (resolve-alias ctx ns)]
      [(symbol (name (:target-ns alias)) (name local-name)) alias]
      [local-name nil])
    (if-let [binding (resolve-binding ctx local-name)]
      [(:fq-name binding) binding]
      [local-name nil])))

(defn- indexed-bindings [bindings]
  (into {} (map (fn [b] [(name (:local-name b)) b]) bindings)))

(defn create-context
  "Creates fresh context with the given custom symbol resolutions
   and namespace exports"
  [symbol-resolution ns-exports]
  {:pre [(map? symbol-resolution)
         (map? ns-exports)]}
  (let [bindings (->> built-in/clojure-core-exports
                      (map #(create-binding % (symbol "clojure.core" (name %))))
                      (indexed-bindings))]
    (-> {:current-ns     "user"
         :scope          (->Scope nil bindings)
         :aliases        {}
         :sym-resolution symbol-resolution
         :mode           :eval
         :recur-target   nil}
        (map->Context))))

(defn set-ns
  "Resets the namespace for the given context"
  [ctx ns-name aliases bindings]
  {:pre [(ctx? ctx)
         (string? ns-name)
         (coll? aliases)
         (every? #(instance? Alias %) aliases)
         (coll? bindings)
         (every? #(instance? Binding %) bindings)]}
  (assoc ctx :current-ns ns-name
             :scope (->Scope nil (indexed-bindings bindings))
             :aliases (into {} (map (fn [a] [(name (:local-name a)) a]) aliases))))

(def ^:private built-in-exports
  {'clojure.core built-in/clojure-core-exports})

(defn get-ns-exports
  "Returns list of fully qualified exports for the given
   namespace or `nil` if exports are not known"
  [ctx ns-name]
  {:pre [(simple-symbol? ns-name)]}
  (or (get built-in-exports ns-name)
      (get (:ns-exports ctx) ns-name)))

;;
;; Core analyzers and specs
;;

(declare analyze-with default-node-analyzer node-satisfies? get-node-analyzer get-form-analyzer)

(defn analysis-ex
  "Constructs new analysis exception with message and lazy position"
  [get-position message]
  {:pre [(ifn? get-position)
         (string? message)]}
  (AnalysisException. get-position message))

(defn- ex->position [^AnalysisException ex]
  (let [pos (.getPosition ex)]
    (assert (pos-int? (:line pos)))
    (assert (pos-int? (:col pos)))
    pos))

(def ^:private feature-analyze-order
  {":default" 1
   ":cljs"    2
   ":clj"     3})

(defn- analyze-reader-cond [inner-analyzer ctx [_ list]]
  (letfn [(reader-cond-list-analyzer [ctx [_ & forms]]
            (loop [[[feat val] & rem] (->> (partition 2 forms)
                                           (sort-by (comp feature-analyze-order ffirst)))
                   ctx ctx
                   res (transient [:list])]
              (if feat
                (let [[ctx' feat'] (analyze-with default-node-analyzer ctx feat)
                      [ctx' val'] (analyze-with inner-analyzer ctx' val)]
                  (recur rem ctx' (conj! (conj! res feat') val')))
                [ctx (persistent! res)])))]
    (let [[ctx' list'] (analyze-with reader-cond-list-analyzer ctx list)]
      [ctx' [:reader-cond list']])))

(defn- analyze-meta-nodes [ctx nodes]
  (when (some? nodes)
    (loop [ctx ctx
           [node & xs] nodes
           result (transient [])]
      (if (some? node)
        (case (first node)
          (:space :newline :comment) (recur ctx xs (conj! result node))
          (:meta :discard) (let [[ctx' node'] (analyze-with default-node-analyzer ctx node)]
                             (recur ctx' xs (conj! result node'))))
        [ctx (seq (persistent! result))]))))

(defn- reader-cond-satisfies? [predicate [_ [_ & forms]]]
  (loop [ok? (not (empty? forms))
         [feat val & rem] forms]
    (if (and feat ok?)
      (recur (node-satisfies? predicate val) rem)
      ok?)))

(declare get-node-analyzer get-form-analyzer)

(defn default-node-analyzer
  "Analyzer that analyzes the given node by using registered
   default node analysis functions"
  [ctx node]
  (let [analyze-f (get-node-analyzer node)]
    (analyze-f ctx node)))

(defn node-satisfies?
  "Returns boolean whether the node satisfies the given
   predicate or not"
  [predicate node]
  {:pre [(node? node)
         (ifn? predicate)]}
  (if (= :reader-cond (first node))
    (reader-cond-satisfies? predicate node)
    (predicate node)))

(defn analyze-with
  "Analyzes the given node and its meta nodes using the supplied
   analyzer function and context"
  [analyzer ctx node]
  {:pre [(ifn? analyzer)
         (ctx? ctx)
         (node? node)]}
  (as-> [ctx (transient (meta node))] state
        (let [[ctx m] state]
          (if-let [[ctx' nodes] (analyze-meta-nodes ctx (:pre m))]
            [ctx' (assoc! m :pre nodes)]
            state))
        (try
          (let [[ctx m] state
                [ctx' node'] (if (= :reader-cond (first node))
                               (analyze-reader-cond analyzer ctx node)
                               (analyzer ctx node))
                _ (assert (ctx? ctx') (str "invalid result context while analyzing ast node: " (node->source node)))
                _ (assert (vector? node') (str "invalid result node while analyzing ast node: " (node->source node)))
                m' (if-some [node-m (meta node')]
                     (reduce-kv assoc! m node-m)
                     m)]
            [ctx' m' node'])
          (catch AnalysisException ex
            (when (= :eval (:mode ctx))
              (warn (ex->position ex) (.getMessage ex)))
            (let [[ctx m] state
                  [ctx' node'] (loop [ctx ctx
                                      [child & xs] (next node)
                                      result (transient [(first node)])]
                                 (if (some? child)
                                   (let [[ctx' child'] (analyze-with default-node-analyzer ctx child)]
                                     (recur ctx' xs (conj! result child')))
                                   [ctx (persistent! result)]))
                  m' (assoc! m :formatting :whitespace)]
              [ctx' m' node'])))
        (let [[ctx m node] state]
          (if-let [[ctx' nodes] (analyze-meta-nodes ctx (:hidden m))]
            [ctx' (assoc! m :hidden nodes) node]
            state))
        (let [[ctx m node] state]
          (if-let [[ctx' nodes] (analyze-meta-nodes ctx (:post m))]
            [ctx' (assoc! m :post nodes) node]
            state))
        (let [[ctx-out m node'] state
              node-out (with-meta node' (persistent! m))]
          (assert (let [no-nils (fn no-nils [node]
                                  (->> (map #(if (vector? %) (no-nils %) %) node)
                                       (filterv some?)))]
                    (= (no-nils node-out) node))
                  (str "analyzed node does not match with node " (node->source node)
                       "\n analyzed-node: " (pr-str node-out)
                       "\n ast-node:      " (pr-str node)))
          [ctx-out node-out])))

(defn get-node-type [ctx node]
  (first node))

(defn get-literal-content [ctx node]
  (second node))

(defn add-as-ns-binding-analyzer
  "(Post) analyzer that adds the given symbol node as fully
   qualified binding using the current namespace for binding's
   ns part"
  [ctx node]
  {:pre [(= :symbol (first node))]}
  (let [local-name (symbol (second node))
        fq-name (symbol (:current-ns ctx) (name local-name))
        b (create-binding local-name fq-name)]
    [(add-binding ctx b)
     (vary-meta node #(dissoc (assoc % :binding b) :fq-name))]))

(defn add-as-local-binding-analyzer
  "(Post) analyzer that adds the given symbol node as an
   unqualified binding"
  [ctx node]
  {:pre [(or (= :symbol (first node))
             (= :keyword (first node)))]}
  (let [fq-name (symbol (second node))
        local-name (symbol (name fq-name))
        b (create-binding local-name local-name)]
    [(add-binding ctx b)
     (vary-meta node #(dissoc (assoc % :binding b) :fq-name))]))

(defn type= [expected-type]
  "Creates a `(ctx, node) -> bool` predicate that matches
   the given node type"
  {:pre [(keyword? expected-type)]}
  (fn type-matches-expected? [ctx node]
    (= expected-type (get-node-type ctx node))))

(defn val= [expected-val]
  "Creates a `(ctx, node) -> bool` predicate that matches
   the given (literal) value"
  (fn value-matches-expected? [_ node]
    (= expected-val (second node))))

(defn lexical-scope-spec
  "Creates spec that runs the given specs inside their own
   lexical scope"
  [& specs]
  {:pre [(seq specs)]}
  (let [on-ctx #(fn [_ ^Acc input]
                  (Acc. (% (.-ctx input)) (.-remaining input) (.-result input)))]
    (s/seq
      (s/custom (on-ctx push-lexical-scope))
      (if (= 1 (count specs))
        (first specs)
        (apply s/seq specs))
      (s/custom (on-ctx pop-lexical-scope)))))

(defn node-pred-spec
  "Creates generic node spec that matches the first remaining child
   node using the given predicate and runs the given inner spec/analyzer
   to its children"
  [predicate spec-name expectation spec-or-analyzer]
  {:pre [(ifn? predicate)
         (string? spec-name)
         (string? expectation)
         (or (s/spec? spec-or-analyzer)
             (ifn? spec-or-analyzer))]}
  (let [analyzer (if (s/spec? spec-or-analyzer) (s/as-analyzer spec-or-analyzer) spec-or-analyzer)
        err-msg-nil (str "expected " spec-name)
        err-msg-not-expected (str "expected " spec-name " to be " expectation)]
    (reify Spec
      (name [_] spec-name)
      (run [_ parent input _]
        (let [ctx (.-ctx ^Acc input)
              rem (.-remaining ^Acc input)
              res (.-result ^Acc input)
              node (first rem)]
          (cond
            (nil? node)
            (s/error parent nil err-msg-nil)
            (node-satisfies? predicate node)
            (let [[ctx' n'] (analyze-with analyzer ctx node)]
              (Acc. ctx' (next rem) (conj! res n')))
            (= :reader-cond-splice (first node))
            ; Techically splicing reader conditional should be like normal reader conditional...
            ; However supporting it would make layouting extremely complex, hence just bailing
            ; out here (at least for now)
            (s/error parent node "imo does not support splicing reader conditional here")
            :else (s/error parent node err-msg-not-expected)))))))

(defn node-spec
  "Creates generic node spec that matches the first remaining child
   node based on its type and runs the given inner spec/analyzer
   to its children"
  [expected-type spec-name spec-or-analyzer]
  {:pre [(keyword? expected-type)]}
  (let [pred (fn node-type-equals? [node]
               (= (first node) expected-type))]
    (node-pred-spec pred spec-name (str expected-type) spec-or-analyzer)))

(defn- node-spec-factory* [predicate expectation]
  (fn node-spec
    ([spec-name] (node-spec spec-name nil))
    ([spec-name post-analyzer]
     {:pre [(string? spec-name)
            (or (nil? post-analyzer)
                (ifn? post-analyzer))]}
     (let [analyzer (s/as-analyzer default-node-analyzer post-analyzer)]
       (node-pred-spec predicate spec-name expectation analyzer)))))

(def any-node-spec
  "Spec that matches any node"
  (node-spec-factory* (fn [_] true) "any node"))

(def simple-symbol-node-spec
  "Spec that matches a simple symbol node"
  (let [pred (fn simple-symbol-node? [node]
               (and (= :symbol (first node))
                    (simple-symbol-str? (second node))))]
    (node-spec-factory* pred "simple symbol")))

(def symbol-node-spec
  "Spec that matches a symbol node"
  (node-spec-factory* #(= :symbol (first %)) "symbol"))

(def keyword-node-spec
  "Spec that matches a keyword node"
  (node-spec-factory* #(= :keyword (first %)) "keyword"))

(def list-node-spec
  "Spec that matches a list node"
  (node-spec-factory* #(= :list (first %)) "list"))

(def map-node-spec
  "Spec that matches a map node"
  (node-spec-factory* #(= :map (first %)) "map"))

(def set-node-spec
  "Spec that matches a set node"
  (node-spec-factory* #(= :set (first %)) "set"))

(def vector-node-spec
  "Spec that matches a vector node"
  (node-spec-factory* #(= :vector (first %)) "vector"))

(def string-node-spec
  "Spec that matches a string node"
  (node-spec-factory* #(= :string (first %)) "string"))

(def number-node-spec
  "Spec that matches a number node"
  (node-spec-factory* #(= :number (first %)) "number"))

(def boolean-node-spec
  "Spec that matches a boolean node"
  (node-spec-factory* #(= :boolean (first %)) "boolean"))

(defn body-expr-spec
  "Creates a spec that accepts any node and annotates it
   with `:body-expr true` metadata"
  [spec-name]
  (any-node-spec spec-name (fn [ctx node] [ctx (vary-meta node assoc :body-expr true)])))

(defn body-exprs-spec*
  "Creates a spec that accepts 0..n any nodes and annotates them
   with `:body-expr true` metadata"
  [spec-name]
  (s/* (body-expr-spec spec-name)))

(declare recur-binding-form-spec)

(defn local-name-binding-spec
  "Spec for local name binding nodes"
  [spec-name]
  (simple-symbol-node-spec spec-name add-as-local-binding-analyzer))

(defn seq-binding-spec
  "Spec for destructurable seq binding e.g `[foo bar & lol]`"
  [spec-name]
  (let [value-is #(let [err-msg (format "expected '%s'" (str %))]
                    (fn [parent ^Acc input]
                      (let [[_ value :as node] (first (.-remaining input))]
                        (when (not= value %)
                          (s/error parent node err-msg)))))]
    (node-spec :vector spec-name
      (s/seq (s/* (s/recursive #'recur-binding-form-spec))
             (s/? (s/seq (s/check (value-is "&"))
                         (symbol-node-spec "rest-marker")
                         (s/recursive #'recur-binding-form-spec)))
             (s/? (s/seq (s/check (value-is ":as"))
                         (keyword-node-spec ":as")
                         (local-name-binding-spec "seq-name")))))))

(defn map-binding-spec
  "Spec for destructurable map binding"
  [spec-name]
  (let [basic-map-binding? (fn [_ [node-type]]
                             (or (= :symbol node-type)
                                 (= :vector node-type)
                                 (= :map node-type)))
        ns-keys-binding? (fn [_ [node-type value]]
                           (and (= :keyword node-type)
                                (boolean (re-find #"/(keys|syms)$" value))))
        special-keyword-spec (fn [binding-type]
                               (keyword-node-spec (name binding-type) (fn [ctx node] [ctx (vary-meta node assoc :binding-type binding-type)])))]
    (node-spec :map spec-name
      (s/* (s/choose
             (val= ":keys") (s/seq (special-keyword-spec :keys)
                                   (node-spec :vector "keys"
                                     (s/* (s/choose
                                            (type= :symbol) (symbol-node-spec "symbol" add-as-local-binding-analyzer)
                                            (type= :keyword) (keyword-node-spec "keyword" add-as-local-binding-analyzer)))))
             (val= ":syms") (s/seq (special-keyword-spec :syms)
                                   (node-spec :vector "syms"
                                     (s/* (symbol-node-spec "symbol" add-as-local-binding-analyzer))))
             (val= ":strs") (s/seq (special-keyword-spec :strs)
                                   (node-spec :vector "strs"
                                     (s/* (simple-symbol-node-spec "symbol" add-as-local-binding-analyzer))))
             (val= ":as") (s/seq (special-keyword-spec :as)
                                 (local-name-binding-spec "map-name"))
             (val= ":or") (s/seq (special-keyword-spec :or)
                                 (map-node-spec "defaults-map"))
             ns-keys-binding? (s/seq (special-keyword-spec :ns-keys)
                                     (node-spec :vector "keys" (s/* (local-name-binding-spec "key-name"))))
             basic-map-binding? (s/recursive #'recur-binding-form-spec))))))

(def ^:private recur-binding-form-spec
  (let [local-name? (fn [_ [node-type content]]
                      (and (= :symbol node-type) (not= "&" content)))
        seq-binding? (type= :vector)
        map-binding? (type= :map)]
    (s/choose
      local-name? (local-name-binding-spec "simple-name")
      seq-binding? (seq-binding-spec "vector-destructuring")
      map-binding? (map-binding-spec "map-destructuring"))))

(defn binding-form-spec
  "Spec for any destructurable form:
    - symbol
    - seq
    - map"
  []
  recur-binding-form-spec)

(defn binding-pair-spec
  "Spec for (destructruable) binding + init-expr pair"
  []
  (s/seq (binding-form-spec)
         (any-node-spec "init-expr")))

(defn bindings-vec-spec
  "Spec for a vector of binding + init-expr pairs"
  [spec-name]
  {:pre [(string? spec-name)]}
  (let [bindings-analyzer (s/as-analyzer (s/* (binding-pair-spec)))
        analyzer (fn [ctx [_ & children :as node]]
                   (when-not (even? (count children))
                     (throw (analysis-ex #(start-of node) "bindings must contain even number of forms")))
                   (bindings-analyzer ctx node))]
    (node-spec :vector spec-name analyzer)))

(defn- fallback-node-analyzer [ctx [type & children]]
  (loop [ctx ctx
         [child & rem] children
         analyzed (transient [type])]
    (cond
      (vector? child) (let [[ctx' child'] (analyze-with default-node-analyzer ctx child)]
                        (recur ctx' rem (conj! analyzed child')))
      (string? child) (recur ctx rem (conj! analyzed child))
      :else [ctx (persistent! analyzed)])))

(defn- list-node-analyzer [ctx [_ i :as node]]
  (if (and (not= :quote (:mode ctx))
           (= :symbol (get-node-type ctx i)))
    (let [local-name (symbol (get-literal-content ctx i))
          invocation (first (resolve-fq-name ctx local-name))
          form-name (get (:sym-resolution ctx) invocation invocation)
          analyzer (get-form-analyzer form-name)
          [ctx' node'] (analyzer ctx node)]
      [ctx' (vary-meta node' assoc :invocation invocation :resolve-as form-name)])
    (fallback-node-analyzer ctx node)))

(defn- symbol-node-analyzer [ctx [_ content :as node]]
  (let [local-name (symbol content)
        fq-name (first (resolve-fq-name ctx local-name))]
    [ctx (with-meta node {:fq-name fq-name})]))

(defn- literal-node-analyzer [ctx node]
  [ctx (with-meta node nil)])

(defn- quote-node-analyzer [ctx [_ quoted]]
  (let [current-mode (:mode ctx)
        [ctx' quoted'] (analyze-with default-node-analyzer (set-mode ctx :quote) quoted)]
    [(set-mode ctx' current-mode)
     [:quote quoted']]))

(defn- syntax-quote-node-analyzer [ctx [_ quoted]]
  (let [current-mode (:mode ctx)
        [ctx' quoted'] (analyze-with default-node-analyzer (set-mode ctx :syntax-quote) quoted)]
    [(set-mode ctx' current-mode)
     [:syntax-quote quoted']]))

(def ^:private quote-form-analyzer
  (let [form-analyzer (-> (s/seq (symbol-node-spec "invocation")
                                 (any-node-spec "quoted-expr"))
                          (s/as-analyzer))]
    (fn [ctx node]
      (let [current-mode (:mode ctx)
            [ctx' node'] (form-analyzer (set-mode ctx :quote) node)]
        [(set-mode ctx' current-mode) node']))))

(def ^:private do-form-analyzer
  (-> (s/seq (symbol-node-spec "invocation")
             (s/* (any-node-spec "body-expr" (fn [ctx node] [ctx (vary-meta node assoc :body-expr true)]))))
      (s/as-analyzer)))

(def ^:private type->node-analyzer
  (->> [:string :number :boolean :keyword :string :char :regex :nil]
       (map (fn [literal-type] [literal-type literal-node-analyzer]))
       (into {:list         list-node-analyzer
              :symbol       symbol-node-analyzer
              :quote        quote-node-analyzer
              :syntax-quote syntax-quote-node-analyzer})))

(defonce ^:private invocation->form-analyzer
  (volatile! {'quote quote-form-analyzer
              'do    do-form-analyzer}))

(defn get-node-analyzer
  "Returns analyzer for the given node"
  [node]
  {:pre [(node? node)]}
  (get type->node-analyzer (first node) fallback-node-analyzer))

(defn get-form-analyzer
  "Returns analyzer for the given form name"
  [form-name]
  {:pre [(symbol? form-name)]}
  (get @invocation->form-analyzer form-name fallback-node-analyzer))

(defn set-form-analyzer!
  "Registers analyzer for the given fully qualified form name"
  [form-name analyzer]
  {:pre [(symbol? form-name)
         (ifn? analyzer)]}
  (vswap! invocation->form-analyzer assoc form-name analyzer))
