(ns imo.util
  (:require [clojure.string :as string]))

(defn split-lines
  "Like string/split-lines but handles also special case where
   `s` contains only empty lines."
  [s]
  {:pre [(string? s)]}
  (string/split s #"\n" -1))


(defmacro match
  "Returns matched nodes if the given value expression matches to the given
   match expression or nil if there is no match.

   Notation:

     _   = any node type
     ..? = zero or one matches
     ..* = zero or more matches
     ..+ = at least one match

   For example:

     (let [m (match nodes '[_ symbol string? map? list*])]
       (println m))

   Matches the following form

     (ns foo.bar
       (:require [clojure.string :as string])
       (:import (java.util Date))

   ...and returns (simplified):

     [ns foo.bar nil nil [(:require ...) (:import ...)]]

   "
  [val-expr match-expr]
  (assert (= 2 (count match-expr)))
  (assert (= 'quote (first match-expr)))
  (assert (vector? (second match-expr)))
  (let [value (gensym "value")
        result (gensym "result")
        m (fn m [[e & rem]]
            (if (some? e)
              (let [expr-s (str e)
                    v (gensym)
                    optional? (or (string/ends-with? expr-s "*")
                                  (string/ends-with? expr-s "?"))
                    multi? (or (string/ends-with? expr-s "*")
                               (string/ends-with? expr-s "+"))
                    expr-s (string/replace expr-s #"[*+?]$" "")
                    matches? (if (= "_" expr-s)
                               `(some? ~v)
                               `(= ~(keyword expr-s) (first ~v)))]
                (if multi?
                  (let [loop-res (gensym)
                        loop-val (gensym)]
                    `(let [[~v ~value]
                           (loop [~loop-res (transient [])
                                  ~loop-val ~value]
                             (let [~v (first ~loop-val)]
                               (if ~matches?
                                 (recur (conj! ~loop-res ~v) (next ~loop-val))
                                 [(persistent! ~loop-res) ~loop-val])))]
                       (when ~(if optional? true `(not-empty ~v))
                         (let [~result (conj! ~result ~v)]
                           ~(m rem)))))
                  `(let [~v (first ~value)]
                     (if ~matches?
                       (let [~result (conj! ~result ~v)
                             ~value (next ~value)]
                         ~(m rem))
                       ~(when optional?
                          `(let [~result (conj! ~result nil)]
                             ~(m rem)))))))
              `(persistent! ~result)))]
    `(let [~value ~val-expr
           ~result (transient [])]
       ~(m (second match-expr)))))