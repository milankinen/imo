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

  (defn- for* [[_ for bindings body]]
    (fn
      ([] (| [for bindings]
             [1 body]))
      ([n] nil)))

  (defn- kv [[k v]])

  (deflayout ::map
    (fn [[_ & kvs]]
      (if (>= 6 (count kvs))
        (let [[k1 v1 k2 v2 k3 v3] kvs]
          (alt [k1 v1 k2 v2 k3 v3]
               (| [k1 (shrinkable v1)]
                  [k2 (shrinkable v2)]
                  [k3 (shrinkable v3)])
               (| (| k1
                     (shrinkable v1))
                  (| k2
                     (shrinkable v2))
                  (| k3
                     (shrinkable v3))))))))


  (l/deflayout ::defn
    (fn [[_ defn fname ?doc-str ?attr-map params ?pre-post-map & body-exprs]
         (if (or ?attr-map ?doc-str)
           (| [defn fname]
              [1 (| ?doc-str
                    ?attr-map
                    params
                    ?pre-post-map
                    (apply | body-exprs))])
           (let [fhead (alt [defn fname params]
                            (| [defn fname]
                               [1 params]))
                 fbody (| ?pre-post-map
                          (apply | body-exprs))])
           (| [fhead]
              [1 fbody]))]
      (fn
        ([] (| [for bindings]
               [1 body]))
        ([n] nil))))

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
