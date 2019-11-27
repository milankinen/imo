(ns imo.formatting
  (:refer-clojure :exclude [format])
  (:import (java.util LinkedList)
           (java.io Writer)))

(defmulti -fmt (fn [ctx _] (:style ctx)))

(defmethod -fmt :default [ctx _]
  (throw (AssertionError. (str "No formatting defined for style: " (:style ctx)))))

(defrecord Block [content
                  lines
                  length
                  shift
                  absolute])

(declare block*)

(defn- string->block [content]
  {:pre [(string? content)]}
  (let [n (.length content)
        [length lines shift]
        (loop [index 0
               col 0
               lines 0
               max-len 0]
          (if (< index n)
            (let [ch (.charAt content index)]
              (if (identical? \n ch)
                (recur (inc index) 0 (inc lines) (max max-len col))
                (recur (inc index) (inc col) lines max-len)))
            [(max max-len col) lines col]))]
    (->Block content
             lines
             length
             shift
             (pos? lines))))

;;
;; Public stuff
;;

(defn format
  "Transforms the given AST node into output block"
  [ctx node]
  {:pre [(vector? node)
         (map? ctx)]}
  (when-some [content (-fmt ctx node)]
    (block* (list content))))

(defn block*
  "Creates an output block from the given contents."
  [contents]
  {:pre [(sequential? contents)]}
  (loop [[x & xs :as rem] contents
         res (transient [])
         lines 0
         col-shift 0
         absolute? false]
    (if (seq rem)
      (cond
        (nil? x)
        (recur xs res lines col-shift absolute?)

        ; Leaf block
        (string? x)
        (if-not (.isEmpty x)
          (let [block (string->block x)
                shift (:shift block)
                absolute? (:absolute block)]
            (recur xs (conj! res block) lines (+ col-shift shift) absolute?))
          (recur xs res lines col-shift absolute?))

        ; Already processed block
        (instance? Block x)
        (let [lines' (+ lines (:lines x))
              shift' (if-not (:absolute x)
                       (+ col-shift (:shift x))
                       (:shift x))]
          (recur xs (conj! res x) lines' shift' (:absolute x)))

        ; Potential multiline block to align
        (vector? x)
        (case (count x)
          0 (recur xs res lines col-shift absolute?)
          1 (recur (cons (first x) xs) res lines col-shift absolute?)
          (let [x' (filter not-empty (map #(block* (if (string? %) [%] %)) x))]
            (case (count x')
              0 (recur xs res lines col-shift absolute?)
              1 (recur (cons (first x') xs) res lines col-shift absolute?)
              (let [aligned-content (vec x')
                    total-lines (->> (map :lines aligned-content)
                                     (reduce + (dec (count aligned-content))))
                    max-length (reduce max 0 (map :length aligned-content))
                    last-row (last aligned-content)
                    last-shift (:shift last-row)
                    last-absolute? (:absolute last-row)
                    block (->Block aligned-content total-lines max-length last-shift last-absolute?)
                    lines' (+ lines total-lines)
                    shift' (if-not last-absolute?
                             (+ col-shift last-shift)
                             last-shift)]
                (recur xs (conj! res block) lines' shift' last-absolute?)))))

        ; Group of blocks without alignment
        (sequential? x)
        (case (count x)
          0 (recur xs res lines col-shift absolute?)
          1 (recur (cons (first x) xs) res lines col-shift absolute?)
          (recur (cons (block* x) xs) res lines col-shift absolute?))

        :else (throw (AssertionError. (str "Non-supported block type: " (type x)))))
      (let [content (persistent! res)]
        (case (count content)
          0 nil
          1 (first content)
          (->Block (seq content)
                   lines
                   (reduce max col-shift (map :length content))
                   col-shift
                   absolute?))))))

(defn block
  "Same as block* but takes contents as variadic arguments."
  [& contents]
  (block* contents))

(defn block->source
  "Transforms the given output block into source code."
  [block]
  {:pre  [(or (instance? Block block)
              (nil? block))]
   :post (string? %)}
  (letfn [(prs [indent content out]
            (cond
              (string? content)
              (do (.addLast out content)
                  (+ indent (.length content)))
              (vector? content)
              (let [lf (str "\n" (.repeat " " indent))]
                (if (seq content)
                  (loop [[x & xs] (map :content content)]
                    (if (seq xs)
                      (do (prs indent x out)
                          (.addLast out lf)
                          (recur xs))
                      (prs indent x out)))
                  indent))
              (sequential? content)
              (loop [[x & xs] (map :content content)
                     i indent]
                (if (some? x)
                  (recur xs (prs i x out))
                  i))
              :else (throw (AssertionError. "Invalid output block"))))]
    (if (some? block)
      (let [out (LinkedList.)]
        (prs 0 (:content block) out)
        (String/join "" out))
      "")))

(defmethod print-method Block [block ^Writer writer]
  (.write writer (block->source block)))
