(ns imo.analysis
  (:require [imo.analysis.core :as a]
            [imo.analysis.context :refer [ctx?] :as ctx]
            [imo.analysis.spec :refer [->Err ->State] :as s]
            [imo.analysis.built-in :as built-in]
            [imo.logger :refer [timed warn]]
            [imo.util :refer [node? start-of]]
            [clojure.string :as string])
  (:import (imo.analysis.spec State)))

(def ^:private built-in-exports
  {'clojure.core built-in/clojure-core-exports})

(def ^:private ^:dynamic *exports* built-in-exports)

;; Utils

(defn- as-ns-binding [ctx node]
  {:pre [(= :symbol (first node))]}
  (let [local-name (symbol (second node))
        fq-name (symbol (:current-ns ctx) (name local-name))
        b (ctx/create-binding local-name fq-name)]
    [(ctx/with-binding ctx b)
     (vary-meta node #(dissoc (assoc % :binding b) :fq-name))]))

(defn- as-local-binding [ctx node]
  {:pre [(or (= :symbol (first node))
             (= :keyword (first node)))]}
  (let [fq-name (symbol (second node))
        local-name (symbol (name fq-name))
        b (ctx/create-binding local-name local-name)]
    [(ctx/with-binding ctx b)
     (vary-meta node #(dissoc (assoc % :binding b) :fq-name))]))

(defn- post-on-ctx [f]
  {:pre [(fn? f)]}
  (fn ctx-post-transform [ctx node]
    [(f ctx) node]))

(defn- post-on-node [f]
  {:pre [(fn? f)]}
  (fn node-post-transform [ctx node]
    [ctx (f node)]))

(defn- type= [expected-type]
  {:pre [(keyword? expected-type)]}
  (fn type-matches-expected? [ctx node]
    (= expected-type (a/get-node-type ctx node))))

(defn- is-exactly [expected-type expected-value]
  (let [err-msg (format "expected '%s'" (str expected-value))]
    (fn [parent ^State input]
      (let [[type value :as node] (s/state->next-node input)]
        (when (or (not= expected-type type)
                  (not= expected-value value))
          (s/error parent node err-msg))))))

(defn- on-ctx [f]
  {:pre [(fn? f)]}
  (fn [_ ^State input]
    (s/with-ctx input (f (s/state->ctx input)))))


;; Default analyzers

(declare invocation->analyzer)

(defmethod a/default-node-analyzer :list [ctx [_ i :as node]]
  (if (and (not= :quote (:mode ctx))
           (= :symbol (a/get-node-type ctx i)))
    (let [local-name (symbol (a/get-literal-content ctx i))
          invocation (first (ctx/resolve-fq-name ctx local-name))
          resolve-as (get (:sym-resolution ctx) invocation invocation)
          analyzer (invocation->analyzer resolve-as)
          [ctx' node'] (analyzer ctx node)]
      [ctx' (vary-meta node' assoc :invocation invocation :resolve-as resolve-as)])
    (a/default-analyzer ctx node)))

(defmethod a/default-node-analyzer :symbol [ctx [_ content :as node]]
  (let [local-name (symbol content)
        fq-name (first (ctx/resolve-fq-name ctx local-name))]
    [ctx (with-meta node {:fq-name fq-name})]))

(doseq [literal-type [:string :number :boolean :keyword :string :char :regex :nil]]
  (defmethod a/default-node-analyzer literal-type [ctx node]
    [ctx (with-meta node nil)]))

(defmethod a/default-node-analyzer :symbol [ctx [_ content :as node]]
  (let [local-name (symbol content)
        fq-name (first (ctx/resolve-fq-name ctx local-name))]
    [ctx (with-meta node {:fq-name fq-name})]))

(doseq [quote-type [:quote :syntax-quote]]
  (defmethod a/default-node-analyzer quote-type [ctx [_ quoted]]
    (let [current-mode (:mode ctx)
          [ctx' quoted'] (a/analyze-with a/default-node-analyzer (ctx/with-mode ctx quote-type) quoted)]
      [(ctx/with-mode ctx' current-mode)
       [quote-type quoted']])))

(def ^:private quote-form-analyzer
  (let [form-analyzer (-> (s/seq (s/symbol-node "invocation")
                                 (s/any-node "quoted-expr"))
                          (s/as-analyzer))]
    (fn [ctx node]
      (let [current-mode (:mode ctx)
            [ctx' node'] (form-analyzer (ctx/with-mode ctx :quote) node)]
        [(ctx/with-mode ctx' current-mode) node']))))



;; Specs

(defn- with-lexical-scope [& specs]
  {:pre [(seq specs)]}
  (s/seq
    (s/custom (on-ctx ctx/push-lexical-scope))
    (if (= 1 (count specs))
      (first specs)
      (apply s/seq specs))
    (s/custom (on-ctx ctx/pop-lexical-scope))))

(defn- body-expr-node [name]
  (s/any-node name (post-on-node #(vary-meta % assoc :body-expr true))))

(def ^:private body-expr-spec
  (body-expr-node "body-expr"))

(declare binding-form-spec)

(def ^:private local-name-binding-spec
  (s/simple-symbol-node "local-name" as-local-binding))

(def ^:private seq-binding-spec
  (s/node :vector "seq-binding"
    (s/seq (s/* (s/recursive #'binding-form-spec))
           (s/? (s/seq (s/check (is-exactly :symbol "&"))
                       (s/symbol-node "rest-marker")
                       (s/recursive #'binding-form-spec)))
           (s/? (s/seq (s/check (is-exactly :keyword ":as"))
                       (s/keyword-node "as-symbol")
                       local-name-binding-spec)))))

(def ^:private map-binding-spec
  (let [basic-map-binding? (fn [_ [node-type]]
                             (or (= :symbol node-type)
                                 (= :vector node-type)
                                 (= :map node-type)))
        ns-keys-binding? (fn [_ [node-type content]]
                           (and (= :keyword node-type)
                                (boolean (re-find #"/(keys|syms)$" content))))
        == (fn [s] (fn [_ [_ content]] (= s content)))
        special-keyword (fn [binding-type]
                          (s/keyword-node (name binding-type) (post-on-node #(vary-meta % assoc :binding-type binding-type))))]
    (s/node :map "map-binding"
      (s/* (s/choose
             (== ":keys") (s/seq (special-keyword :keys)
                                 (s/node :vector "keys"
                                   (s/* (s/choose
                                          (type= :symbol) (s/symbol-node "symbol" as-local-binding)
                                          (type= :keyword) (s/keyword-node "keyword" as-local-binding)))))
             (== ":syms") (s/seq (special-keyword :syms)
                                 (s/node :vector "syms"
                                   (s/* (s/symbol-node "symbol" as-local-binding))))
             (== ":strs") (s/seq (special-keyword :strs)
                                 (s/node :vector "strs"
                                   (s/* (s/simple-symbol-node "symbol" as-local-binding))))
             (== ":as") (s/seq (special-keyword :as)
                               local-name-binding-spec)
             (== ":or") (s/seq (special-keyword :or)
                               (s/map-node "defaults-map"))
             ns-keys-binding? (s/seq (special-keyword :ns-keys)
                                     (s/node :vector "keys" (s/* local-name-binding-spec)))
             basic-map-binding? (s/recursive #'binding-form-spec))))))

(def ^:private binding-form-spec
  (let [local-name? (fn [_ [node-type content]]
                      (and (= :symbol node-type) (not= "&" content)))
        seq-binding? (type= :vector)
        map-binding? (type= :map)]
    (s/choose
      local-name? local-name-binding-spec
      seq-binding? seq-binding-spec
      map-binding? map-binding-spec)))

(def ^:private binding-spec
  (s/seq binding-form-spec (s/any-node "init-expr")))

(def ^:private bindings-vec-spec
  (let [bindings-analyzer (s/as-analyzer (s/* binding-spec))
        analyzer (fn [ctx [_ & children :as node]]
                   (when-not (even? (count children))
                     (throw (a/analysis-ex #(start-of node) "bindings must contain even number of forms")))
                   (bindings-analyzer ctx node))]
    (s/node :vector "bindings" analyzer)))

(def ^:private param-spec
  (s/* (s/symbol-node "param")))

(def ^:private fn-tail-spec
  (with-lexical-scope
    (s/named "params" seq-binding-spec)
    (s/? (s/map-node "pre-post-map"))
    (s/* body-expr-spec)))

(def ^:private signature-spec
  (s/node :list "signature"
    (s/as-analyzer fn-tail-spec (post-on-node #(vary-meta % assoc :signature true)))))

(def ^:private fn-spec
  (s/seq (s/symbol-node "invocation")
         (s/? (s/simple-symbol-node "fname" as-ns-binding))
         (s/choose
           (type= :vector) fn-tail-spec
           (type= :list) (s/+ signature-spec))))

(def ^:private letfn-spec
  (with-lexical-scope
    (s/symbol-node "invocation")
    (s/node :vector "fnspecs"
      (s/* (s/node :list "fnspec"
             (s/seq (s/simple-symbol-node "fname" as-local-binding)
                    (s/choose
                      (type= :vector) fn-tail-spec
                      (type= :list) (s/+ signature-spec))))))
    (s/* body-expr-spec)))

(def ^:private let-spec
  (with-lexical-scope
    (s/symbol-node "invocation")
    bindings-vec-spec
    (s/* body-expr-spec)))

(def ^:private seq-expr-bindings-spec
  (let [== (fn [s] (fn [_ [_ content]] (= content s)))]
    (s/node :vector "seq-expr-bindings"
      (s/* (s/seq binding-spec
                  (s/* (s/choose
                         (== ":let") (s/seq (s/keyword-node ":let") bindings-vec-spec)
                         (== ":while") (s/seq (s/keyword-node ":while") body-expr-spec)
                         (== ":when") (s/seq (s/keyword-node ":when") body-expr-spec)
                         binding-spec)))))))

(def ^:private for-spec
  (with-lexical-scope
    (s/symbol-node "invocation")
    seq-expr-bindings-spec
    body-expr-spec))

(def ^:private do***-spec
  (with-lexical-scope
    (s/symbol-node "invocation")
    seq-expr-bindings-spec
    (s/* body-expr-spec)))

(def ^:private do-spec
  (s/seq (s/symbol-node "invocation")
         (s/* body-expr-spec)))

(def ^:private defn-spec
  (s/seq (s/symbol-node "invocation")
         (s/simple-symbol-node "fname" as-ns-binding)
         (s/? (s/string-node "doc-string"))
         (s/? (s/map-node "attrs-map"))
         (s/choose
           (type= :vector) fn-tail-spec
           (type= :list) (s/seq (s/+ signature-spec)
                                (s/? (s/map-node "attrs-map"))))))

(def ^:private def-form-analyzer
  (let [with-doc-str-analyzer (-> (s/seq (s/symbol-node "invocation")
                                         (s/simple-symbol-node "name" as-ns-binding)
                                         (s/string-node "doc-string")
                                         (s/? (body-expr-node "value")))
                                  (s/as-analyzer))
        no-doc-str-analyzer (-> (s/seq (s/symbol-node "invocation")
                                       (s/simple-symbol-node "name" as-ns-binding)
                                       (s/custom (fn [_ ^State input]
                                                   (let [res (s/state->result input)]
                                                     (s/with-result input (conj! res nil)))))
                                       (s/? (body-expr-node "value")))
                                (s/as-analyzer))]
    (fn [ctx [_ & children :as node]]
      (if (>= (count children) 4)
        (with-doc-str-analyzer ctx node)
        (no-doc-str-analyzer ctx node)))))

(def ^:private if-spec
  (s/seq (s/symbol-node "invocation")
         (s/any-node "test")
         (body-expr-node "then")
         (s/? (body-expr-node "else"))))

(def ^:private ns-require-spec
  (let [== (fn [s] (fn [_ [_ content]] (= s content)))
        as-spec (s/seq (s/keyword-node ":as")
                       (s/simple-symbol-node "alias"))
        mark-renames (fn [ctx node]
                       (let [renames (-> (next node)
                                         (map (comp symbol second))
                                         (partition-all 2)
                                         (map vec)
                                         (into {}))]
                         [ctx (vary-meta node :rename renames)]))
        rename-spec (s/seq (s/keyword-node ":rename")
                           (s/node :map "renamings"
                             (-> (s/* (s/seq (s/simple-symbol-node "old-name")
                                             (s/simple-symbol-node "new-name")))
                                 (s/as-analyzer mark-renames))))
        mark-as-refer (fn [ctx node]
                        [ctx (vary-meta node assoc :refer (symbol (second node)))])
        refered-sym-spec (s/simple-symbol-node "refered-symbol" mark-as-refer)
        collect-vec-refers (fn [ctx node]
                             (let [refers (set (map (comp :refer meta) (next node)))]
                               [ctx (vary-meta node assoc :refer refers)]))
        refer-vec-spec (s/node :vector "refers" (s/as-analyzer (s/* refered-sym-spec) collect-vec-refers))
        mark-as-refer-all (fn [ctx node] [ctx (vary-meta node assoc :refer :all)])
        refer-all-spec (s/keyword-node ":all" mark-as-refer-all)
        refer-spec (s/seq (s/keyword-node ":refer")
                          (s/choose
                            (== ":all") refer-all-spec
                            (type= :vector) refer-vec-spec))
        collect-vec-libspec (fn [ctx [_ lib-ns & opts :as node]]
                              (let [lib-ns-sym (symbol (second lib-ns))
                                    libspec
                                    (loop [libspec {:namespace lib-ns-sym}
                                           [[o v] & remaining] (partition 2 opts)]
                                      (if o
                                        (recur (case (second o)
                                                 ":refer" (if-let [refer (:refer (meta v))]
                                                            (assoc libspec :refer refer)
                                                            libspec)
                                                 ":as" (assoc libspec :as (symbol (second v)))
                                                 ":rename" (if-let [rename (:rename (meta v))]
                                                             (assoc libspec :rename rename)
                                                             libspec))
                                               remaining)
                                        (update libspec
                                                :refer #(if (= % :all)
                                                          (or (get *exports* lib-ns-sym)
                                                              (warn nil "couldn't find :all exports for " lib-ns-sym))
                                                          %))))]
                                [ctx (vary-meta node assoc :libspec libspec)]))
        vec-libspec (s/node :vector "libspec"
                      (-> (s/seq (s/simple-symbol-node "lib")
                                 (s/* (s/choose
                                        (== ":refer") refer-spec
                                        (== ":rename") rename-spec
                                        (== ":as") as-spec)))
                          (s/as-analyzer collect-vec-libspec)))
        collect-sym-libspec (fn [ctx node]
                              (let [libspec {:namespace (symbol (second node))}]
                                [ctx (vary-meta node assoc :libspec libspec)]))
        sym-libspec (s/simple-symbol-node "libname" collect-sym-libspec)
        collect-prefix-list (fn [ctx [_ prefix-node & libspec-nodes :as node]]
                              (let [prefix (str (second prefix-node) ".")
                                    libspecs (->> (map (comp :libspec meta) libspec-nodes)
                                                  (map (fn [libspec]
                                                         (update libspec :namespace #(symbol (str prefix %))))))]
                                [ctx (vary-meta node assoc :libspecs libspecs)]))
        prefix-list (s/node :list "prefix-list"
                      (-> (s/seq (s/simple-symbol-node "prefix")
                                 (s/+ (s/choose
                                        (type= :vector) vec-libspec
                                        (type= :symbol) sym-libspec)))
                          (s/as-analyzer collect-prefix-list)))
        flag (s/keyword-node "flag" (fn [ctx node] [ctx (vary-meta node assoc :flag (symbol (second node)))]))]
    (s/seq (s/keyword-node ":require")
           (s/+ (s/choose
                  (type= :vector) vec-libspec
                  (type= :symbol) sym-libspec
                  (type= :list) prefix-list
                  (type= :keyword) flag)))))

(def ^:private ns-refer-clojure-spec
  (let [== (fn [s] (fn [_ [_ content]] (= s content)))
        syms-as (fn [key]
                  (fn [ctx [_ & children :as node]]
                    (let [syms (map (comp symbol second) children)]
                      [ctx (vary-meta node assoc key syms)])))
        only-spec (s/seq (s/keyword-node ":only")
                         (s/node :vector "requires"
                           (-> (s/* (s/simple-symbol-node "require"))
                               (s/as-analyzer (syms-as :only)))))
        exclude-spec (s/seq (s/keyword-node ":exclude")
                            (s/node :vector "requires"
                              (-> (s/* (s/simple-symbol-node "require"))
                                  (s/as-analyzer (syms-as :exclude)))))
        rename-spec (s/seq (s/keyword-node ":rename")
                           (s/node :map "renames"
                             (-> (s/* (s/seq (s/simple-symbol-node "old-name")
                                             (s/simple-symbol-node "new-name")))
                                 (s/as-analyzer (fn [ctx [_ & children :as node]]
                                                  (let [renames (->> (map (comp symbol second) children)
                                                                     (partition 2)
                                                                     (map vec)
                                                                     (into {}))]
                                                    [ctx (vary-meta node assoc :rename renames)]))))))]
    (s/seq (s/keyword-node ":refer-clojure")
           (s/* (s/choose
                  (== ":only") only-spec
                  (== ":exclude") exclude-spec
                  (== ":rename") rename-spec)))))


(def ^:private ns-import-spec
  (let [single-class-spec (s/simple-symbol-node
                            "class-name"
                            (fn [ctx node]
                              (let [parts (string/split (second node) #"\.")
                                    [pkg cls] (if (= 1 (count parts))
                                                [nil (first parts)]
                                                [(string/join "." (pop parts))
                                                 (peek parts)])
                                    import {:package pkg
                                            :class   cls}]
                                [ctx (vary-meta node assoc :imports [import])])))
        class-list-spec (s/node :list "package-list"
                          (-> (s/seq (s/simple-symbol-node "package-name")
                                     (s/+ (s/simple-symbol-node "")))
                              (s/as-analyzer
                                (fn [ctx [_ pkg-node & class-nodes :as node]]
                                  (let [pkg (second pkg-node)
                                        classes (map second class-nodes)
                                        imports (map (fn [c] {:package pkg :class c}) classes)]
                                    [ctx (vary-meta node assoc :imports imports)])))))]
    (s/seq (s/keyword-node ":import")
           (s/* (s/choose
                  (type= :symbol) single-class-spec
                  (type= :list) class-list-spec)))))

(def ^:private ns-use-spec
  (s/seq (s/keyword-node ":use")
         (s/custom (fn [parent input]
                     (warn (start-of parent) ":use is not supported (at least for now), use :require instead")
                     input))
         (s/* (s/any-node "libspec"))))

(def ^:private ns-gen-class-spec
  (s/seq (s/keyword-node ":gen-class")
         (s/* (s/any-node "option"))))

(def ^:private ns-load-spec
  (s/seq (s/keyword-node ":load")
         (s/* (s/string-node "lib"))))

(def ^:private ns-refer-spec
  (s/seq (s/keyword-node ":refer")
         (s/simple-symbol-node "lib")
         (s/custom (fn [parent input]
                     (warn (start-of parent) ":refer is not supported (at least for now)")
                     input))
         (s/* (s/any-node "filter"))))

(def ^:private ns-clause-analyzer
  (let [== (fn [s] (fn [_ [_ content]] (= s content)))
        collect-meta (fn [ctx [_ clause & args :as node]]
                       [ctx (case (second clause)
                              nil node
                              ":require" (let [requires (mapcat
                                                          #(let [m (meta %)]
                                                             (cond
                                                               (:libspecs m) (:libspecs m)
                                                               (:libspec m) [(:libspec m)]
                                                               :else nil))
                                                          args)]
                                           (vary-meta node assoc :clause :require :requires requires))
                              ":refer-clojure" (let [spec (->> (map #(select-keys (meta %) [:only :exclude :rename]) args)
                                                               (reduce merge {}))]
                                                 (vary-meta node assoc :clause :refer-clojure :spec spec))
                              ":import" (let [imports (mapcat (comp :imports meta) args)]
                                          (vary-meta node assoc :clause :import :imports imports))
                              (let [clause (keyword (subs (second clause) 1))]
                                (vary-meta node assoc :clause clause)))])]
    (-> (s/choose
          (== ":require") ns-require-spec
          (== ":refer-clojure") ns-refer-clojure-spec
          (== ":import") ns-import-spec
          (== ":gen-class") ns-gen-class-spec
          (== ":use") ns-use-spec
          (== ":refer") ns-refer-spec
          (== ":load") ns-load-spec)
        (s/as-analyzer collect-meta))))

(def ^:private ns-analyzer
  (letfn [(in-ns [ctx [_ _ ns-name-node _ _ & clause-nodes :as node]]
            (let [ns-name (second ns-name-node)
                  requires (mapcat (comp :requires meta) clause-nodes)
                  aliases (keep (fn [{:keys [alias namespace]}]
                                  (some-> alias (ctx/create-alias namespace)))
                                requires)
                  clj-spec (first (keep #(let [m (meta %)]
                                           (when (= :refer-clojure (:clause m))
                                             (:spec m)))
                                        clause-nodes))
                  clj-exports (as-> built-in/clojure-core-exports exports
                                    (if-let [only (seq (:only clj-spec))]
                                      (filter (comp (set (map name only)) name) exports)
                                      exports)
                                    (if-let [exclude (seq (:exclude clj-spec))]
                                      (remove (comp (set (map name exclude)) name) exports)
                                      exports)
                                    (let [rename (or (:rename clj-spec) {})]
                                      (map #(if-let [local (get rename (symbol (name %)))]
                                              (ctx/create-binding local %)
                                              (ctx/create-binding (symbol (name %)) %))
                                           exports)))
                  imports (->> (mapcat (comp :imports meta) clause-nodes)
                               (mapcat (fn [{:keys [package class]}]
                                         (let [fq-s (if package
                                                      (str package "." class)
                                                      class)]
                                           [; class name
                                            (ctx/create-binding (symbol class) (symbol fq-s))
                                            ; ctor
                                            (ctx/create-binding (symbol (str class "."))
                                                                (symbol (str fq-s ".")))]))))
                  bindings (-> (mapcat (fn [{:keys [refer rename namespace]}]
                                         (map #(let [local-name (get rename % %)
                                                     fq-name (symbol (str namespace) (str %))]
                                                 (ctx/create-binding local-name fq-name))
                                              refer))
                                       requires)
                               (concat clj-exports imports))
                  ctx-in-ns (ctx/in-ns ctx ns-name aliases bindings)]
              [ctx-in-ns node]))]
    (-> (s/seq (s/symbol-node "invocation")
               (s/simple-symbol-node "ns-name")
               (s/? (s/string-node "doc-string"))
               (s/? (s/map-node "attr-map"))
               (s/* (s/node :list "ns-clause" ns-clause-analyzer)))
        (s/as-analyzer in-ns))))

(def ^:private invocation->analyzer
  (let [if-analyzer (s/as-analyzer if-spec)
        do-analyzer (s/as-analyzer do-spec)
        quote-analyzer quote-form-analyzer
        let-analyzer (s/as-analyzer let-spec)
        defn-analyzer (s/as-analyzer defn-spec)
        fn-analyzer (s/as-analyzer fn-spec)
        letfn-analyzer (s/as-analyzer letfn-spec)
        for-analyzer (s/as-analyzer for-spec)
        doseq-analyzer (s/as-analyzer do***-spec)]
    (fn [invocation]
      (case invocation
        def def-form-analyzer
        if if-analyzer
        do do-analyzer
        quote quote-analyzer
        clojure.core/ns ns-analyzer
        clojure.core/let let-analyzer
        clojure.core/defn defn-analyzer
        clojure.core/fn fn-analyzer
        clojure.core/letfn letfn-analyzer
        clojure.core/for for-analyzer
        clojure.core/doseq doseq-analyzer
        a/default-analyzer))))

(defn- create-clj-context []
  (let [bindings (->> built-in/clojure-core-exports
                      (map #(ctx/create-binding (symbol (name %)) %)))
        sym-resolution built-in/clojure-core-symbol-resolution]
    (ctx/create-context bindings sym-resolution)))

;;
;;

(defn analyze-ast
  "Runs static analysis to the given input ast and add annotates
   the returned ast nodes with analysis results"
  [ast]
  {:pre [(node? ast)
         (= :$ (first ast))]}
  (timed "analysis"
    (let [ctx (create-clj-context)]
      (-> (a/analyze-with a/default-node-analyzer ctx ast)
          (second)))))