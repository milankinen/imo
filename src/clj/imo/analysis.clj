(ns imo.analysis
  (:require [imo.analysis.core :as a]
            [imo.analysis.context :refer [ctx?] :as ctx]
            [imo.analysis.spec :refer [->Err ->State] :as s]
            [imo.analysis.built-in :as built-in]
            [imo.logger :refer [timed]]
            [imo.util :refer [node? start-of]])
  (:import (imo.analysis.spec State)))

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
  (fn [parent ^State input]
    (let [[type value :as node] (s/state->next-node input)]
      (when (or (not= expected-type type)
                (not= expected-value value))
        (s/error parent node (format "expected '%s'" (str value)))))))

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
                     (throw (a/analysis-ex (start-of node) "bindings must contain even number of forms")))
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
        clojure.core/let let-analyzer
        clojure.core/defn defn-analyzer
        clojure.core/fn fn-analyzer
        clojure.core/letfn letfn-analyzer
        clojure.core/for for-analyzer
        clojure.core/doseq doseq-analyzer
        a/default-analyzer))))

(defn- create-clj-context []
  (let [bindings (a/exports->bindings built-in/clojure-core-exports)
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