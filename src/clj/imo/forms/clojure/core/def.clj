(ns imo.forms.clojure.core.def
  (:require [imo.analysis.core :as a]
            [imo.analysis.exception :refer [analysis-ex]]))

(a/defspec ::name (a/named ::a/ns-sym-binding "name"))
(a/defspec ::doc-str (a/named (a/? ::a/string) "doc-string"))
(a/defspec ::init-expr (a/named (a/? ::a/body-expr) "init expression"))

(a/defspec ::def+doc [::a/symbol ::name ::doc-str ::init-expr])
(a/defspec ::def [::a/symbol ::name ::init-expr])

(a/defform 'def #(case (dec (count %2))
                   (2 3) (a/analyze ::def %1 %2)
                   4 (a/analyze ::def+doc %1 %2)
                   (throw (analysis-ex "def must be form (def name doc-str? init-expr?)" %2))))

(a/defform 'clojure.core/defonce #(a/analyze ::def %1 %2))

(comment

  (repl/explain*
    (def foo
      "this is a doc-str"
      123))

  (repl/explain*
    (defonce lol
      "bal"))

  '-)
