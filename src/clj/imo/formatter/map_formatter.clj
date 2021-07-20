(ns imo.formatter.map-formatter
  (:require [imo.formatter.core :as f]
            [imo.util :refer [may-fit-one-line? maxl]]
            [imo.layout.builder :as b]))

(defn format-one-line
  "{:a 1 :b 2}"
  [[_ & children :as node] offset target-width alternative]
  (when (or (may-fit-one-line? node (- target-width offset))
            (zero? alternative))
    (if-let [all-children (seq (concat children (:children (meta node))))]
      (loop [b (b/coll-builder offset target-width alternative "{" "}" false)
             [x & xs] (interpose 1 all-children)]
        (if (and x b)
          (recur (b/add! b (f/format-outer-node-one-line x (b/offset b) target-width alternative)) xs)
          (b/build! b)))
      "{}")))

(defn format-aligned
  "{:key     1
    :another 2
    :third   3}"
  [[_ & children :as node] offset target-width alternative]
  (let [map-b (b/coll-builder offset target-width alternative "{" "}" false)
        kv-pairs (partition-all 2 children)
        k-offset (inc offset)
        ;; first create builders for all key-value pairs, so that
        ;; we can calculate justification for values
        b+v-for-kvs
        (loop [key-bs []
               [[k v] & xs] kv-pairs]
          (if k
            (when-some [kv-b (-> (b/builder k-offset target-width alternative)
                                 (b/add! (f/format-outer-node-align-meta k k-offset target-width alternative)))]
              (recur (conj key-bs [kv-b v]) xs))
            key-bs))
        ;; then add the actual key values
        map-b
        (if (seq b+v-for-kvs)
          ;; offset for all values = max key offset + 1
          (let [v-offset (inc (reduce maxl 0 (map (comp b/offset first) b+v-for-kvs)))]
            (loop [b map-b
                   [[kv-b v :as kv] & xs] b+v-for-kvs
                   first-kv? true]
              (if (and b kv)
                (when-some [kv-layout (-> kv-b
                                          (b/add! (- v-offset (b/offset kv-b)))
                                          (b/add! (f/format-outer-node-align-meta v v-offset target-width alternative))
                                          (b/build!))]
                  (recur (-> (if first-kv? b (b/add! b :break))
                             (b/add! kv-layout))
                         xs
                         false))
                b)))
          map-b)
        ;; then add meta children if any
        meta-children (:children (meta node))
        map-b
        (if (and map-b meta-children)
          (loop [b (if (seq kv-pairs) (b/add! map-b :break) map-b)
                 [x & xs] meta-children]
            (if (and b x)
              (let [layout (f/format-outer-node-align-meta x k-offset target-width alternative)]
                (recur (-> (b/add! b :break)
                           (b/add! layout))
                       xs))
              b))
          map-b)]
    (b/build! map-b)))

(defmethod f/format-inner-node :map [node offset target-width alternative]
  (or (format-one-line node offset target-width (max alternative 8))
      (when (< alternative 8)
        (format-aligned node offset target-width alternative))))

(comment

  (fmt* '(def opts {:arglists '([coll])
                    :doc      "Return the last item in coll, in linear time"
                    :added    "1.0"
                    :static   true}))


  -)