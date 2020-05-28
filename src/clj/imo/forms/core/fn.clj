(ns imo.forms.core.fn
  (:require [imo.analysis.spec :as s]
            [imo.analysis :as a]))

(def fn-tail-spec
  (a/lexical-scope-spec
    (a/seq-binding-spec "params")
    (s/? (a/map-node-spec "pre-post-map"))
    (a/body-exprs-spec* "body-expr")))

(def signature-spec
  (a/node-spec :list "signature"
    (s/as-analyzer fn-tail-spec (fn [ctx node] [ctx (vary-meta node assoc :signature true)]))))

(def fn-spec
  (s/seq (a/symbol-node-spec "invocation")
         (s/? (a/simple-symbol-node-spec "fname" a/add-as-ns-binding-analyzer))
         (s/choose
           (a/type= :vector) fn-tail-spec
           (a/type= :list) (s/+ signature-spec))))

(def defn-spec
  (s/seq (a/symbol-node-spec "invocation")
         (a/simple-symbol-node-spec "fname" a/add-as-ns-binding-analyzer)
         (s/? (a/string-node-spec "doc-string"))
         (s/? (a/map-node-spec "attrs-map"))
         (s/choose
           (a/type= :vector) fn-tail-spec
           (a/type= :list) (s/seq (s/+ signature-spec)
                                  (s/? (a/map-node-spec "attrs-map"))))))

(a/add-form-analyzer! 'clojure.core/fn (s/as-analyzer fn-spec))
(a/add-form-analyzer! 'clojure.core/defn (s/as-analyzer defn-spec))
