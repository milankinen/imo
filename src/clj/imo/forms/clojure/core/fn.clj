(ns imo.forms.clojure.core.fn
  (:require [imo.analysis.core :as a]))

;; shared
(a/defspec ::params (a/named ::a/seq-binding "function params"))
(a/defspec ::pre-post-map (a/named (a/? ::a/map) "pre-post map"))
(a/defspec ::fn-body (a/* ::a/body-expr))
(a/defspec ::params+body (a/in-scope ::params ::pre-post-map ::fn-body))
(a/defspec ::signature (a/named (a/list-node ::params+body) "function signature"))
(a/defspec ::attr-map (a/named (a/? ::a/map) "attrs map"))
(a/defspec ::fn-tail (a/alt ::params+body [(a/+ ::signature) ::attr-map]))

;; fn
(a/defspec ::fn [::a/symbol (a/? ::a/local-sym-binding) ::fn-tail])
(a/defform 'clojure.core/fn #(a/analyze ::fn %1 %2))

;; defn
(a/defspec ::fname (a/named ::a/ns-sym-binding "function name"))
(a/defspec ::doc-str (a/? ::a/string))
(a/defspec ::defn [::a/symbol ::fname ::doc-str ::attr-map ::fn-tail])
(a/defform 'clojure.core/defn #(a/analyze ::defn %1 %2))
(a/defform 'clojure.core/defn- #(a/analyze ::defn %1 %2))


(comment

  (repl/explain*
    '(fn [foo {:keys [bar] :or {bar 1}}]
       (inc foo)
       (inc bar)))

  (repl/explain*
    '(fn [foo & [inc]]
       (inc foo)
       (inc bar)))

  (repl/explain*
    '(fn lol [foo bar]
       (lol foo bar)))

  (repl/explain*
    '(fn lol
       ([foo] (inc foo))
       ([foo bar] (+ foo bar))))

  (repl/explain*
    '(defn lol
       "bal"
       [] 123))

  (repl/explain*
    '(defn lol
       ([] 123)
       ([a] a)))

  -)
