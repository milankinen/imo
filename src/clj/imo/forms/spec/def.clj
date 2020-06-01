(ns imo.forms.spec.def
  (:require [imo.analysis.spec :as s]
            [imo.analysis :as a]
            [imo.layout :as l]))

(def ^:private def-analyzer
  (-> (s/seq (a/symbol-node-spec "invocation")
             (s/choose
               (a/type= :keyword) (a/keyword-node-spec "spec-name")
               (a/type= :symbol) (a/symbol-node-spec "spec-name"))
             (a/any-node-spec "spec-form"))
      (s/as-analyzer)))

(doto 'clojure.spec.alpha/def
  (a/set-form-analyzer! def-analyzer)
  (l/mark-as-groupable!))
