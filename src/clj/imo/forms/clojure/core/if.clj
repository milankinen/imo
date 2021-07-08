(ns imo.forms.clojure.core.if
  (:require [imo.analysis.core :as a]))

(a/defspec ::condition (a/named ::a/any "condition"))
(a/defspec ::then (a/named ::a/body-expr "'then' expression"))
(a/defspec ::else (a/? ::a/body-expr))

(a/defspec ::if [::a/symbol ::condition ::then ::else])

(a/defform 'if #(a/analyze ::if %1 %2))


(comment

  (repl/explain*
    (if foo
      "lol"
      "bal"))

  (repl/explain*
    (if (= 1 2)
      "lol"))

  '-)