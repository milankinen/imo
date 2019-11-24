(ns imo.ast
  (:refer-clojure :exclude [vector? symbol? string? number? nil?
                            boolean? char? keyword? tagged-literal?
                            list? map?])
  (:require [clojure.core :as core]))

(defn- node= [type]
  (fn [[t :as node]]
    (and (core/vector? node)
         (= t type))))

(def newline? (node= :newline))
(def space? (node= :space))
(def nil? (node= :nil))
(def number? (node= :number))
(def boolean? (node= :boolean))
(def string? (node= :string))
(def char? (node= :char))
(def keyword? (node= :keyword))
(def symbol? (node= :symbol))
(def meta? (node= :meta))
(def comment? (node= :comment))
(def quote? (node= :quote))
(def syntax-quote? (node= :syntax_quote))
(def deref? (node= :deref))
(def unquote? (node= :unquote))
(def unquote-splice? (node= :unquote_splice))
(def var-quote? (node= :var_quote))
(def regex? (node= :regex))
(def ignore? (node= :ignore))
(def tagged-literal? (node= :tagged_literal))
(def reader-cond? (node= :reader_cond))
(def anon-fn? (node= :anon_fn))
(def list? (node= :list))
(def vector? (node= :vector))
(def map? (node= :map))
(def ns-map? (node= :ns_map))
(def symbolic-val? (node= :symbolic_val))

