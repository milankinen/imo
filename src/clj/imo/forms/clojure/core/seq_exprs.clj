(ns imo.forms.clojure.core.seq-exprs
  (:require [imo.analysis.core :as a]))

;; shared
(a/defspec ::init-expr (a/named ::a/any "init expression"))
(a/defspec ::binding [::a/any-binding ::init-expr])

(a/defspec ::seq-expr-bindings
  (a/vec-node
    (a/* [::binding (a/* (a/alt [":let" ::a/bindings-vec]
                                [":while" ::a/body-expr]
                                [":when" ::a/body-expr]
                                ::binding))])))

;; for
(a/defspec ::for [::a/symbol ::seq-expr-bindings ::a/body-expr])
(a/defform 'clojure.core/for #(a/analyze ::for %1 %2))

;; doseq
(a/defspec ::doseq [::a/symbol ::seq-expr-bindings (a/* ::a/body-expr)])
(a/defform 'clojure.core/doseq #(a/analyze ::doseq %1 %2))

;; dotimes
(a/defspec ::dotimes [::a/symbol ::seq-expr-bindings (a/* ::a/body-expr)])
(a/defform 'clojure.core/dotimes #(a/analyze ::dotimes %1 %2))


(comment

  (repl/explain*
    (for [a [1 2 3]
          :when (odd? a)
          :let [b (inc a)]]
      b))

  (repl/explain*
    (for [_ [1 2 3]
          a [1 2 3]
          :let [b (inc a)]]
      b))

  -)
