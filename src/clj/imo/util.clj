(ns imo.util
  (:require [clojure.string :as string]
            [clojure.set :as set])
  (:import (imo AstNode Util)))

(def whitespace-node-types
  #{:space
    :newline
    :comment})

(def reader-node-types
  #{:reader-cond
    :reader-cond-splice})

(def evaluable-node-types
  #{:anon-fn
    :boolean
    :char
    :deref
    :discard
    :keyword
    :list
    :map
    :meta
    :nil
    :ns-map
    :number
    :quote
    :regex
    :set
    :string
    :symbol
    :symbolic-val
    :syntax-quote
    :tagged-literal
    :unquote
    :unquote-splice
    :var-quote
    :vector})

(def terminal-node-types
  #{:boolean
    :char
    :keyword
    :nil
    :number
    :regex
    :string
    :symbol
    :symbolic-val})

(def conjuction-node-types
  (set/difference evaluable-node-types terminal-node-types))

(defn split-lines
  "Like string/split-lines but handles also special case where
   `s` contains only empty lines."
  [s]
  {:pre [(string? s)]}
  (string/split s #"\n" -1))

(defn ^String spaces [n]
  {:pre [(integer? n)]}
  (if (pos-int? n)
    (Util/spaces n)
    ""))

(defn ^long maxl
  "Fast version of clojure.core/max, for longs only."
  {:inline (fn [a b] `(Util/maxLong ~a ~b))}
  [a b]
  (Util/maxLong a b))

(defn node? [x]
  (and (vector? x)
       (true? (:imo/node (meta x)))))

(defn begin-chars [node-type]
  {:pre [(keyword? node-type)]}
  (AstNode/getBeginChars node-type))

(defn all-begin-chars []
  (set (AstNode/getAllBeginChars)))

(defn end-chars [node-type]
  {:pre [(keyword? node-type)]}
  (AstNode/getEndChars node-type))

(defn all-end-chars []
  (set (AstNode/getAllEndChars)))

(defn simple-name-str? [s]
  {:pre [(string? s)]}
  (Util/isSimpleNameStr s))

(defn node->source [node]
  {:pre [(node? node)]}
  (letfn [(to-str! [^StringBuilder sb node]
            (doseq [n (or (:pre* (meta node)) (:pre (meta node)))]
              (to-str! sb n))
            (when-let [chars (begin-chars (first node))]
              (.append sb ^String chars))
            (doseq [child (concat (next node) (or (:children* (meta node)) (:children (meta node))))]
              (cond
                (vector? child) (to-str! sb child)
                (string? child) (.append sb ^String child)
                (nil? child) nil
                :else (throw (RuntimeException. (str "Invalid node: " (pr-str child))))))
            (when-let [chars (end-chars (first node))]
              (.append sb ^String chars))
            (doseq [n (or (:post* (meta node)) (:post (meta node)))]
              (to-str! sb n)))]
    (let [sb (StringBuilder.)]
      (to-str! sb node)
      (.toString sb))))

(defn start-of [node]
  {:pre [(node? node)]}
  {:line (:line (meta node))
   :col  (:col (meta node))})

(defn end-of [node]
  {:pre [(node? node)]}
  (let [node-s (node->source node)
        line (+ (:line (meta node))
                (count (filter #{\n} node-s)))
        col (+ (:col (meta node))
               (->> (reverse node-s)
                    (take-while (comp not #{\n}))
                    (count)))]
    {:line line :col col}))

(defn- invalid* [node]
  (if (contains? terminal-node-types (first node))
    (:invalid? (meta node))
    (or (:invalid? (meta node))
        (some invalid* (next node)))))

(defn invalid? [node]
  {:pre [(node? node)]}
  (boolean (invalid* node)))

(defn has-children? [node]
  {:pre [(node? node)]}
  (or (> (count node) 1)
      (> (count (:children (meta node))) 0)))

(defn may-fit-one-line? [node line-width]
  {:pre [(node? node)
         (number? line-width)]}
  (let [{:keys [inner-length inner-lines]} (meta node)]
    (and (zero? inner-lines)
         (<= inner-length line-width))))

(defn may-outer-fit-one-line? [node line-width]
  {:pre [(node? node)
         (number? line-width)]}
  (let [{:keys [outer-length outer-lines]} (meta node)]
    (and (zero? outer-lines)
         (<= outer-length line-width))))

(defn may-all-fit-one-line? [nodes line-width]
  {:pre [(number? line-width)]}
  (loop [[x & xs :as nodes] (seq nodes)
         len 0]
    (if nodes
      (if x
        (let [{:keys [inner-length inner-lines]} (meta x)
              len (long (+ len inner-length))]
          (if (and (zero? inner-lines)
                   (<= len line-width))
            (recur xs len)
            false))
        (recur xs len))
      true)))

(defn outer-nodes [node]
  {:pre [(or (node? node)
             (nil? node))]}
  (when node
    (let [{:keys [pre post]} (meta node)]
      (filter some? (concat pre [node] post)))))
