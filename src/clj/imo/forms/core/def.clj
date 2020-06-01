(ns imo.forms.core.def
  (:require [imo.analysis.spec :as s]
            [imo.analysis :as a]
            [imo.layout :as l]))

(def ^:private def-analyzer
  (let [with-doc-str-analyzer (-> (s/seq (a/symbol-node-spec "invocation")
                                         (a/simple-symbol-node-spec "name" a/add-as-ns-binding-analyzer)
                                         (a/string-node-spec "doc-string")
                                         (s/? (a/body-expr-spec "value")))
                                  (s/as-analyzer))
        no-doc-str-analyzer (-> (s/seq (a/symbol-node-spec "invocation")
                                       (a/simple-symbol-node-spec "name" a/add-as-ns-binding-analyzer)
                                       (s/custom (fn [_ input] (update input :result #(conj! % nil))))
                                       (s/? (a/body-expr-spec "value")))
                                (s/as-analyzer))]
    (fn [ctx [_ & children :as node]]
      (if (>= (count children) 4)
        (with-doc-str-analyzer ctx node)
        (no-doc-str-analyzer ctx node)))))

(def ^:private defonce-analyzer
  (-> (s/seq (a/symbol-node-spec "invocation")
             (a/simple-symbol-node-spec "name" a/add-as-ns-binding-analyzer)
             (a/any-node-spec "expr"))
      (s/as-analyzer)))

(doto 'def
  (a/set-form-analyzer! def-analyzer)
  (l/mark-as-groupable!))

(doto 'clojure.core/defonce
  (a/set-form-analyzer! defonce-analyzer)
  (l/mark-as-groupable!))
