(ns imo.forms.clojure.core.let
  (:require [imo.analysis.core :as a]
            [imo.forms.clojure.core.fn :as fn]))

(a/defspec ::let (a/in-scope ::a/symbol ::a/bindings-vec (a/* ::a/body-expr)))
(a/defform 'clojure.core/let #(a/analyze ::let %1 %2))

(a/defspec ::fn-spec (a/list-node [::a/local-sym-binding ::fn/fn-tail]))
(a/defspec ::fn-specs (a/vec-node (a/* ::fn-spec)))
(a/defspec ::letfn [::a/symbol ::fn-specs (a/* ::a/body-expr)])
(a/defform 'clojure.core/letfn #(a/analyze ::letfn %1 %2))
