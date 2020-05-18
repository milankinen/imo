(ns imo.formatting.preserve-formatting
  (:require [clojure.string :as string]
            [imo.util :refer [node? begin-chars end-chars split-lines spaces]])
  (:import (imo Layout)))

(declare format-node! write-node!)

(defn- format-nodes! [sb nodes absolute?]
  (loop [[node & rem :as nodes] (seq nodes)
         absolute? absolute?]
    (if nodes
      (recur rem (format-node! sb node absolute?))
      absolute?)))

(defn- write-node! [^StringBuilder sb [type & children] absolute?]
  (case type
    (:space :number :nil :boolean :symbol :keyword :char :regex)
    (do (.append sb ^String (first children))
        absolute?)
    (:string)
    (let [s (first children)]
      (.append sb s)
      (or absolute? (string/includes? s "\n")))
    (:newline :comment)
    (do (.append sb (first children))
        false)
    (format-nodes! sb children absolute?)))

(defn- format-node! [^StringBuilder sb node absolute?]
  (if (some? node)
    (let [m (meta node)
          t (first node)
          _ (when-not (keyword? t)
              (println node))
          abs? (format-nodes! sb (:pre m) absolute?)
          _ (when-some [chars (begin-chars t)]
              (.append sb ^String chars))
          abs? (write-node! sb node abs?)
          abs? (format-nodes! sb (:hidden m) abs?)
          _ (when-some [chars (end-chars t)]
              (.append sb ^String chars))
          abs? (format-nodes! sb (:post m) abs?)]
      abs?)
    absolute?))

(defn layout-preserve [node]
  {:pre [(node? node)]}
  (let [sb (StringBuilder.)
        absolute? (format-node! sb node false)
        col (:col (meta node))
        lines (split-lines (.toString sb))
        lines (if (> col 1)
                (let [re (re-pattern (str "^[ ]{1," (dec col) "}"))]
                  (mapv #(string/replace % re "") lines))
                (vec lines))
        bits (Layout/toBits
               (> (count lines) 1)
               (count (first lines))
               (count (peek lines))
               (reduce max 0 (map count lines))
               0
               false
               false
               absolute?
               0)]
    (proxy [Layout] [bits nil]
      (kind [] :preserve)
      (inspectChildren [] (seq [(string/join "\n" lines)]))
      (shrink [_ _ _ _]
        (throw (AssertionError. "not shrinkable")))
      (print [^StringBuilder sb offset]
        (let [alignment (spaces offset)]
          (.append sb ^String (first lines))
          (doseq [line (next lines)]
            (.append sb "\n")
            (.append sb alignment)
            (.append sb ^String line)))))))
