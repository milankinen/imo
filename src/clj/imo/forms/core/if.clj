(ns imo.forms.core.if
  (:require [imo.analysis.spec :as s]
            [imo.analysis :as a]))

(def if-spec
  (s/seq (a/symbol-node-spec "invocation")
         (a/any-node-spec "test")
         (a/body-expr-spec "then")
         (s/? (a/body-expr-spec "else"))))

(a/set-form-analyzer! 'if (s/as-analyzer if-spec))
