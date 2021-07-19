(ns imo.formatter.list-formatter
  (:require [imo.formatter.core :refer [format-node]]
            [imo.util :refer [may-fit-one-line? outer-nodes has-children?]]
            [imo.layout :as l]))

(defn format-list-one-line
  "(fname arg-1 arg-2 arg-3)"
  [[_ & children :as node] offset target-width alternative]
  (when (or (may-fit-one-line? node (- target-width offset))
            (zero? alternative))
    (let [items (->> (concat children (:children (meta node)))
                     (mapcat outer-nodes))]
      (loop [b (-> (l/builder offset target-width alternative)
                   (l/append! "(")
                   (l/append! :align))
             [x & xs] (interpose 1 items)]
        (if (and x b)
          (recur (l/append-node-one-line! b x format-node) xs)
          (some-> (l/append! b ")")
                  (l/append! :dealign)
                  (l/build!)))))))

(defn format-list-indet-1
  "(fname arg-1
     arg-2
     arg-3)"
  [[_ fname arg-1 & arg-n* :as node] offset target-width alternative]
  (let [line-1-items (concat (outer-nodes fname) (outer-nodes arg-1))
        line-n*-items (concat (mapcat outer-nodes arg-n*) (:children (meta node)))
        b (loop [b (-> (l/builder offset target-width alternative)
                       (l/append! "(")
                       (l/append! :align-indent))
                 [x & xs] (interpose 1 line-1-items)]
            (if (and x b)
              (recur (l/append-node! b x format-node) xs)
              b))
        b (if (seq line-n*-items)
            (loop [b (if (seq line-1-items) (l/append! b :break) b)
                   [x & xs] (interpose :break line-n*-items)]
              (if (and x b)
                (recur (l/append-node! b x format-node) xs)
                b))
            b)]
    (some-> (l/append! b ")")
            (l/append! :dealign)
            (l/build!))))

(defn format-empty-list [_node offset target-width alternative]
  (when (or (zero? alternative)
            (<= (+ offset 2) target-width))
    "()"))

(defmethod format-node :list [node offset target-width alternative]
  (if (has-children? node)
    (or (format-list-one-line node offset target-width (max alternative 10))
        (when (< alternative 10)
          (format-list-indet-1 node offset target-width alternative)))
    (format-empty-list node offset target-width alternative)))


(comment

  (fmt* '(or (format-list-one-line node offset target-width (max alternative 10))
             (when (< alternative 10)
               (format-list-indet-1 node offset target-width alternative))))


  -)