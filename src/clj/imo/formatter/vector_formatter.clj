(ns imo.formatter.vector-formatter
  (:require [imo.formatter.core :refer [format-node]]
            [imo.util :refer [may-fit-one-line? outer-nodes]]
            [imo.layout :as l]))

(defn- vector-child-nodes [[_ & children :as node]]
  (->> (concat children (:children (meta node)))
       (mapcat outer-nodes)))

(defn format-vector-one-line
  "[a b c d]"
  [node offset target-width alternative]
  (loop [items (transient [])
         offset (inc offset)
         [x & xs :as nodes] (seq (interpose " " (vector-child-nodes node)))]
    (if nodes
      (if (= " " x)
        (recur (conj! items " ") (inc offset) xs)
        (when-some [item (format-node x offset target-width alternative)]
          (recur (conj! items item) (l/next-offset item offset) xs)))
      (l/create ["[" (persistent! items) "]"]))))

(defn format-vector-aligned
  "[a
    b
    c]"
  [node offset target-width alternative]
  (let [offset (inc offset)]
    (loop [items (transient [])
           [x & xs :as nodes] (seq (interpose :break (vector-child-nodes node)))]
      (if nodes
        (if (= :break x)
          (recur (conj! items :break) xs)
          (when-some [item (format-node x offset target-width alternative)]
            (recur (conj! items item) xs)))
        (l/create ["[" :align (persistent! items) "]" :dealign])))))

(defn format-empty-vector [_node offset target-width alternative]
  (when (or (zero? alternative)
            (<= (+ offset 2) target-width))
    "[]"))

(defmethod format-node :vector [node offset target-width alternative]
  (if (or (> (count node) 1)
          (> (count (:children (meta node))) 0))
    (or (when (may-fit-one-line? node (- target-width offset))
          (format-vector-one-line node offset target-width (max alternative 10)))
        (when (< alternative 10)
          (format-vector-aligned node offset target-width alternative)))
    (format-empty-vector node offset target-width alternative)))

(comment
  (format* [1 2 3])
  (format* [1 2 3 [1 2 3] [1 2 3] [1 2 3] [1 2 3] [1 2 3] [1 2 3] [1 2 3] [1 2 3] [1 2 3] [1 2 3]])

  -)