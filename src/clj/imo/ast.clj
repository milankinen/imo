(ns imo.ast
  (:refer-clojure :exclude [vector? symbol? string? number? nil?
                            boolean? char? keyword? tagged-literal?
                            list? map? flatten])
  (:require [clojure.core :as core]))

(defn- node= [type]
  (fn [x] (= type (first x))))

(defn node? [x]
  (true? (:node (meta x))))

; Leaf nodes
(def newline? (node= :newline))
(def space? (node= :space))
(def nil? (node= :nil))
(def number? (node= :number))
(def boolean? (node= :boolean))
(def string? (node= :string))
(def char? (node= :char))
(def keyword? (node= :keyword))
(def symbol? (node= :symbol))
(def regex? (node= :regex))
(def symbolic-val? (node= :symbolic_val))

; Single child "higher order" nodes
(def meta? (node= :meta))
(def comment? (node= :comment))
(def quote? (node= :quote))
(def syntax-quote? (node= :syntax_quote))
(def deref? (node= :deref))
(def unquote? (node= :unquote))
(def unquote-splice? (node= :unquote_splice))
(def var-quote? (node= :var_quote))
(def discard? (node= :discard))

; Multi-child "higher order" nodes
(def tagged-literal? (node= :tagged_literal))
(def reader-cond? (node= :reader_cond))
(def anon-fn? (node= :anon_fn))
(def list? (node= :list))
(def vector? (node= :vector))
(def map? (node= :map))
(def ns-map? (node= :ns_map))


(def children next)

(defn flatten [node]
  (let [node' (vary-meta node assoc :pre nil :post nil :children nil)
        m (meta node)]
    (concat (:pre m) [node'] (:children m) (:post m))))

