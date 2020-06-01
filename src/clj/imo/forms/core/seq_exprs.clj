(ns imo.forms.core.seq-exprs
  (:require [imo.analysis.spec :as s]
            [imo.analysis :as a]))

(def ^:private seq-expr-bindings-spec
  (a/node-spec :vector "seq-expr-bindings"
    (s/* (s/seq (a/binding-pair-spec)
                (s/* (s/choose
                       (a/val= ":let") (s/seq (a/keyword-node-spec ":let")
                                              (a/bindings-vec-spec "bindings"))
                       (a/val= ":while") (s/seq (a/keyword-node-spec ":while")
                                                (a/body-expr-spec "body-expr"))
                       (a/val= ":when") (s/seq (a/keyword-node-spec ":when")
                                               (a/body-expr-spec "body-expr"))
                       (a/binding-pair-spec)))))))

(def ^:private for-spec
  (a/lexical-scope-spec
    (a/symbol-node-spec "invocation")
    seq-expr-bindings-spec
    (a/body-expr-spec "body-expr")))

(def ^:private do***-spec
  (a/lexical-scope-spec
    (a/symbol-node-spec "invocation")
    seq-expr-bindings-spec
    (a/body-exprs-spec* "body-expr")))

(a/set-form-analyzer! 'clojure.core/for (s/as-analyzer for-spec))
(a/set-form-analyzer! 'clojure.core/doseq (s/as-analyzer do***-spec))
