(ns imo.forms.clojure.spec.alpha.def
  (:require [imo.analysis.core :as a]))

(a/defspec ::def [::a/symbol (a/alt ::a/keyword ::a/symbol) ::a/body-expr])
(a/defform 'clojure.spec.alpha/def #(a/analyze ::def %1 %2))
