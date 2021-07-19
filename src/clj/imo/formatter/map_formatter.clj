(ns imo.formatter.map-formatter
  (:require [imo.formatter.core :refer [format-node]]
            [imo.util :refer [may-fit-one-line? outer-nodes has-children? terminal-node-types maxl]]
            [imo.layout :as l]))

(defn format-map-one-line
  "{:a 1 :b 2}"
  [[_ & children :as node] offset target-width alternative]
  (when (or (may-fit-one-line? node (- target-width offset))
            (zero? alternative))
    (let [items (->> (concat children (:children (meta node)))
                     (mapcat outer-nodes))]
      (loop [b (-> (l/builder offset target-width alternative)
                   (l/append! "{")
                   (l/append! :align))
             [x & xs] (interpose 1 items)]
        (if (and x b)
          (if (integer? x)
            (recur (l/append! b x) xs)
            (recur (l/append-node! b x format-node) xs))
          (some-> (l/append! b "}")
                  (l/append! :dealign)
                  (l/build!)))))))

(defn- append-key! [b k]
  (let [nodes (outer-nodes k)]
    (case (= 1 (count nodes))
      1 (if (contains? terminal-node-types (ffirst nodes))
          ;; Common case, literal terminal as a key, e.g. {:foo 1}
          ;; we can't affect to the formatted layout so shortcut here
          (l/append! b (second (first nodes)))
          ;; Rare case, but possible: using collection (or form) as
          ;; a key, e.g. {[1 2] ...}. Format key with default node
          ;; formatter, at least for now
          (l/append-node! b (first nodes) format-node))
      ;; Multiple nodes, e.g. comments and metas and discards -
      ;; add to the separate lines
      (loop [b (l/append! b :align)
             [x & xs] (interpose :break nodes)]
        (if (and b x)
          (recur (l/append-node! b x format-node) xs)
          (l/append! b :dealign))))))

(defn- append-value! [b v]
  (loop [b (l/append! b :align)
         [x & xs] (seq (interpose :break (outer-nodes v)))]
    (if (and b x)
      (recur (l/append-node! b x format-node) xs)
      (l/append! b :dealign))))


(defn format-map-aligned
  "{:key     1
    :another 2
    :third   3}"
  [[_ & children :as node] offset target-width alternative]
  (let [map-b (-> (l/builder offset target-width alternative)
                  (l/append! "{")
                  (l/append! :align))
        kv-pairs (partition-all 2 children)
        k-offset (inc offset)
        key-bs (loop [key-bs []
                      [[k v] & xs] kv-pairs]
                 (if k
                   (when-some [key-b (-> (l/builder k-offset target-width alternative)
                                         (append-key! k))]
                     (recur (conj key-bs [key-b v]) xs))
                   key-bs))
        map-b (when key-bs
                (let [v-offset (when key-bs (inc (reduce maxl 0 (map (comp l/b-offset first) key-bs))))]
                  (loop [b map-b
                         [[key-b v :as kv] & xs] key-bs
                         needs-break? false]
                    (if (and b kv)
                      (recur (-> (if needs-break? (l/append! b :break) b)
                                 (l/append! (l/build! key-b))
                                 ;; Append spaces required for values alignment
                                 (l/append! (- v-offset (l/b-offset key-b)))
                                 (append-value! v))
                             xs
                             true)
                      b))))
        map-b (when map-b
                (loop [b map-b
                       [x & xs] (mapcat outer-nodes (:children (meta node)))
                       needs-break? (pos? (count key-bs))]
                  (if (and b x)
                    (recur (-> (if needs-break? (l/append! b :break) b)
                               (l/append-node! x format-node))
                           xs
                           ;; wtf
                           (boolean true))
                    b)))]
    (some-> map-b
            (l/append!  "}")
            (l/append! :dealign)
            (l/build!))))

(defn format-empty-map [_node offset target-width alternative]
  (when (or (zero? alternative)
            (<= (+ offset 2) target-width))
    "{}"))

(defmethod format-node :map [node offset target-width alternative]
  (if (has-children? node)
    (or (format-map-one-line node offset target-width (max alternative 8))
        (when (< alternative 8)
          (format-map-aligned node offset target-width alternative)))
    (format-empty-map node offset target-width alternative)))

(comment

  (fmt* '(def opts {:arglists '([coll])
                    :doc "Return the last item in coll, in linear time"
                    :added "1.0"
                    :static true}))


  -)