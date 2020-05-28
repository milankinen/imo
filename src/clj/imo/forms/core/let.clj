(ns imo.forms.core.let
  (:require [imo.analysis.spec :as s]
            [imo.analysis :as a]
            [imo.forms.core.fn :refer [fn-tail-spec signature-spec]]))

(def letfn-spec
  (a/lexical-scope-spec
    (a/symbol-node-spec "invocation")
    (a/node-spec :vector "fnspecs"
      (s/* (a/node-spec :list "fnspec"
             (s/seq (a/simple-symbol-node-spec "fname" a/add-as-local-binding-analyzer)
                    (s/choose
                      (a/type= :vector) fn-tail-spec
                      (a/type= :list) (s/+ signature-spec))))))
    (a/body-exprs-spec* "body-expr")))

(def let-spec
  (a/lexical-scope-spec
    (a/symbol-node-spec "invocation")
    (a/bindings-vec-spec "bindings")
    (a/body-exprs-spec* "body-expr")))

(a/add-form-analyzer! 'clojure.core/let (s/as-analyzer let-spec))
(a/add-form-analyzer! 'clojure.core/letfn (s/as-analyzer letfn-spec))
