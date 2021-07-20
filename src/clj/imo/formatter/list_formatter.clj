(ns imo.formatter.list-formatter
  (:require [imo.formatter.core :as f]
            [imo.util :refer [may-fit-one-line?]]
            [imo.layout.builder :as b]
            [imo.layout.core :as l]))

(defn format-one-line
  "(fname arg-1 arg-2 arg-3)"
  [[_ & children :as node] offset target-width alternative]
  (when (or (may-fit-one-line? node (- target-width offset))
            (zero? alternative))
    (if-let [all-children (seq (concat children (:children (meta node))))]
      (loop [b (b/coll-builder offset target-width alternative "(" ")" false)
             [x & xs] (interpose l/space all-children)]
        (if (and x b)
          (let [layout (f/format-outer-node-one-line x (b/offset b) target-width alternative)]
            (recur (b/add! b layout) xs))
          (b/build! b)))
      "()")))

(defn format-indent-arg-1
  "(fname arg-1
     arg-2
     arg-3)"
  [[_ fname arg-1 & arg-n* :as node] offset target-width alternative]
  (let [builder (b/coll-builder offset target-width alternative "(" ")" true)
        ;; first line
        builder (if fname
                  (let [layout (f/format-outer-node-align-meta fname (inc offset) target-width alternative)]
                    (b/add! builder layout))
                  builder)
        builder (if (and arg-1 builder)
                  (let [layout (f/format-outer-node-align-meta arg-1 (inc (b/offset builder)) target-width alternative)]
                    (-> (b/add! builder l/space)
                        (b/add! layout)))
                  builder)
        ;; lines 2...n
        line-n-offset (+ 2 offset)
        builder (when builder
                  (loop [b builder
                         [x & xs] (concat arg-n* (:children (meta node)))]
                    (if (and b x)
                      (let [layout (f/format-outer-node-align-meta x line-n-offset target-width alternative)]
                        (recur (-> (b/add! b :break)
                                   (b/add! layout))
                               xs))
                      b)))]
    (b/build! builder)))

(defn format-aligned
  "(fname arg-1
          arg-2
          arg-3)"
  [[_ fname arg-1 & arg-n* :as node] offset target-width alternative]
  (let [builder (b/coll-builder offset target-width alternative "(" ")" true)
        ;; first line
        builder (if fname
                  (b/add! builder (f/format-outer-node-align-meta fname (inc offset) target-width alternative))
                  builder)
        line-n-offset (some-> builder (b/offset) (inc))
        builder (if (and arg-1 builder)
                  (-> (b/add! builder l/space)
                      (b/add! :align)
                      (b/add! (f/format-outer-node-align-meta arg-1 (b/offset builder) target-width alternative)))
                  builder)
        ;; lines 2...n
        builder (when builder
                  (loop [b builder
                         [x & xs] (concat arg-n* (:children (meta node)))]
                    (if (and b x)
                      (let [layout (f/format-outer-node-align-meta x line-n-offset target-width alternative)]
                        (recur (-> (b/add! b :break)
                                   (b/add! layout))
                               xs))
                      b)))
        ;; finally must dealign the state
        builder (if (and builder arg-1)
                  (b/add! builder :dealign)
                  builder)]
    (b/build! builder)))

;;;;

(defmulti format-form
  (fn [node _offset _target-width _alternative]
    (:resolve-as (meta node))))

(defn- format-default [node offset target-width alternative]
  (or (format-one-line node offset target-width (max alternative 10))
      (when (< alternative 10)
        (format-indent-arg-1 node offset target-width alternative))))

(defmethod format-form :default [node offset target-width alternative]
  (format-default node offset target-width alternative))

(defmethod f/format-inner-node :list [node offset target-width alternative]
  (if-not (:invalid? (meta node))
    (format-form node offset target-width alternative)
    (format-default node offset target-width alternative)))
