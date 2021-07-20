(ns imo.formatter.vector-formatter
  (:require [imo.formatter.core :as f]
            [imo.util :refer [may-fit-one-line?]]
            [imo.layout.builder :as b]))

(defn format-one-line
  "[a b c d]"
  [[_ & children :as node] offset target-width alternative]
  (when (or (zero? alternative)
            (may-fit-one-line? node (- target-width offset)))
    (if-let [children (seq (concat children (:children (meta node))))]
      (loop [b (b/coll-builder offset target-width alternative "[" "]" false)
             [x & xs] (interpose 1 children)]
        (if (and x b)
          (recur (b/add! b (f/format-outer-node-one-line x (b/offset b) target-width alternative)) xs)
          (b/build! b)))
      "[]")))

(defn format-aligned
  "[a
    b
    c]"
  [[_ & children :as node] offset target-width alternative]
  (loop [b (b/coll-builder offset target-width alternative "[" "]" false)
         [x & xs] (->> (concat children (:children (meta node)))
                       (interpose :break))]
    (if (and x b)
      (recur (b/add! b (f/format-outer-node-align-meta x (b/offset b) target-width alternative)) xs)
      (b/build! b))))

(defmethod f/format-inner-node :vector [node offset target-width alternative]
  (or (format-one-line node offset target-width (max alternative 5))
      (when (< alternative 5)
        (format-aligned node offset target-width alternative))))

(comment
  (fmt* [1 2 3])
  (fmt* [1 2 3 [1 2 3] [1 2 3] [1 2 3] [1 2 3] [1 2 3] [1 2 3] [1 2 3] [1 2 3] [1 2 3] [1 2 3]])

  -)