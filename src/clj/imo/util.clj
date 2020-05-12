(ns imo.util
  (:require [clojure.string :as string])
  (:import (imo AstNode)))

(defn split-lines
  "Like string/split-lines but handles also special case where
   `s` contains only empty lines."
  [s]
  {:pre [(string? s)]}
  (string/split s #"\n" -1))

(defn node? [x]
  (and (vector? x)
       (true? (:imo/node (meta x)))))

(defn begin-chars [node-type]
  {:pre [(keyword? node-type)]}
  (AstNode/getBeginChars node-type))

(defn end-chars [node-type]
  {:pre [(keyword? node-type)]}
  (AstNode/getEndChars node-type))

(defn node->source [node]
  {:pre [(node? node)]}
  (letfn [(to-str! [^StringBuilder sb node]
            (doseq [n (:pre (meta node))]
              (to-str! sb n))
            (when-let [chars (begin-chars (first node))]
              (.append sb ^String chars))
            (doseq [child (concat (next node) (:hidden (meta node)))]
              (cond
                (vector? child) (to-str! sb child)
                (string? child) (.append sb ^String child)
                (nil? child) nil
                :else (throw (RuntimeException. (str "Invalid node: " (pr-str child))))))
            (when-let [chars (end-chars (first node))]
              (.append sb ^String chars))
            (doseq [n (:post (meta node))]
              (to-str! sb n)))]
    (let [sb (StringBuilder.)]
      (to-str! sb node)
      (.toString sb))))

(defn start-of [node]
  {:pre [(node? node)]}
  {:start {:line (:line (meta node))
           :col  (:col (meta node))}})

(defn end-of [node]
  {:pre [(node? node)]}
  (let [node-s (node->source node)
        line (+ (:line (meta node))
                (count (filter #{\n} node-s)))
        col (+ (:col (meta node))
               (->> (reverse node-s)
                    (take-while (comp not #{\n}))
                    (count)))]
    {:end {:line line :col col}}))

(defn entire [node]
  (merge (start-of node) (end-of node)))