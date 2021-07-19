(ns imo.formatter.vector-formatter
  (:require [imo.formatter.core :refer [format-node]]
            [imo.util :refer [may-fit-one-line? outer-nodes has-children?]]
            [imo.layout :as l]))

(defn- vector-child-nodes [[_ & children :as node]]
  (->> (concat children (:children (meta node)))
       (mapcat outer-nodes)))

(defn format-vector-one-line
  "[a b c d]"
  [node offset target-width alternative]
  (loop [b (-> (l/builder offset target-width alternative)
               (l/append! "[")
               (l/append! :align))
         [x & xs] (interpose 1 (vector-child-nodes node))]
    (if (and x b)
      (recur (l/append-node-one-line! b x format-node) xs)
      (some-> (l/append! b "]")
              (l/append! :dealign)
              (l/build!)))))

(defn format-vector-aligned
  "[a
    b
    c]"
  [node offset target-width alternative]
  (when (or (may-fit-one-line? node (- target-width offset))
            (zero? alternative))
    (loop [b (-> (l/builder offset target-width alternative)
                 (l/append! "[")
                 (l/append! :align))
           [x & xs] (interpose :break (vector-child-nodes node))]
      (if (and x b)
        (recur (l/append-node! b x format-node) xs)
        (some-> (l/append! b "]")
                (l/append! :dealign)
                (l/build!))))))

(defn format-empty-vector [_node offset target-width alternative]
  (when (or (zero? alternative)
            (<= (+ offset 2) target-width))
    "[]"))

(defmethod format-node :vector [node offset target-width alternative]
  (if (has-children? node)
    (or (format-vector-one-line node offset target-width (max alternative 5))
        (when (< alternative 5)
          (format-vector-aligned node offset target-width alternative)))
    (format-empty-vector node offset target-width alternative)))

(comment
  (fmt* [1 2 3])
  (fmt* [1 2 3 [1 2 3] [1 2 3] [1 2 3] [1 2 3] [1 2 3] [1 2 3] [1 2 3] [1 2 3] [1 2 3] [1 2 3]])

  -)