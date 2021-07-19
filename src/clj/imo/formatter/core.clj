(ns imo.formatter.core
  (:require [imo.util :refer [node? spaces terminal-node-types whitespace-node-types split-lines begin-chars end-chars maxl]]
            [imo.layout :as l])
  (:import (java.util LinkedList)))

(defrecord Context
  [^long target-width
   ^long alternative])

(defn context? [x]
  (instance? Context x))

(def default-ctx
  (map->Context {:target-width 80
                 :alternative  0}))

(defn preserve-format
  "Creates a layout that tries to preserve the original formatting
   as much as possible"
  [node]
  {:pre [(node? node)]}
  (let [alignment (dec (:col (meta node)))]
    (letfn [(get-last-line [^LinkedList lines]
              (.getLast lines))
            (write-lines* [lines nodes]
              (doseq [node nodes]
                (write-lines lines node)))
            (new-line [absolute initially-ignored-spaces]
              ^{:ignored (volatile! initially-ignored-spaces)}
              {:absolute absolute
               :content  (StringBuilder.)})
            (write-lines [^LinkedList lines [node-type :as node]]
              (let [{:keys [pre* post* children*]} (meta node)
                    _ (write-lines* lines pre*)
                    {:keys [^StringBuilder content] :as last-line} (get-last-line lines)
                    ingnored-spaces (:ignored (meta last-line))]
                (when-let [chars (begin-chars node-type)]
                  (.append content chars))
                (cond
                  ;; newline - add new non-absolute line to the result
                  (= :newline node-type)
                  (.add lines (new-line false 0))
                  ;; space - remove heading whitespace up to the column if possible
                  (= :space node-type)
                  (if (pos? (.length content))
                    (.append content ^String (second node))
                    (let [to-ignore-count (maxl 0 (- alignment @ingnored-spaces))
                          pending-count (.length ^String (second node))]
                      (when (> pending-count to-ignore-count)
                        (.append content (spaces (- pending-count to-ignore-count))))))
                  ;; string - split it to lines, add first line to the current line
                  ;; and remaining lines as absolute lines to the result
                  (= :string node-type)
                  (let [[x & xs] (split-lines (second node))]
                    (.append content x)
                    (doseq [x xs]
                      (let [line (new-line true alignment)]
                        (.append ^StringBuilder (:content line) x)
                        (.add lines line))))
                  ;; other terminals and whitespaces - append to the current line
                  (or (contains? terminal-node-types node-type)
                      (contains? whitespace-node-types node-type))
                  (.append content (second node))
                  ;; other nodes - write contents recusively
                  :else (write-lines* lines (remove nil? (next node))))
                (write-lines* lines children*)
                (when-let [chars (end-chars node-type)]
                  (.append ^StringBuilder (:content (get-last-line lines)) chars))
                (write-lines* lines post*)
                lines))]
      ;; Meta nodes are already formatted from the top-level node, so remove
      ;; them before writing line information
      (->> (vary-meta node dissoc :pre* :post* :children*)
           (write-lines (LinkedList. [(new-line false alignment)]))
           (mapv #(update % :content str))
           (l/multiline)))))

(defmulti format-node (fn [node _offset _target-width _alternative] (first node)))

(defmethod format-node :comment [[_ text] _ _ _]
  (l/comment text))

(defmethod format-node :space [node _ _ _]
  ;; All whitespace nodes should be removed before the canonical
  ;; formatting so this implementation should not be called, unless
  ;; there is a bug somewhere.
  (throw (RuntimeException. (str "Tried to format :space node at " (pr-str (select-keys (meta node) [:col :line]))))))

(defmethod format-node :newline [node _ _ _]
  ;; All whitespace nodes should be removed before the canonical
  ;; formatting so this implementation should not be called, unless
  ;; there is a bug somewhere.
  (throw (RuntimeException. (str "Tried to format :newline node at " (pr-str (select-keys (meta node) [:col :line]))))))


(defmethod format-node :default [node offset target-width alternative]
  (let [layout (preserve-format node)]
    (when (or (zero? alternative)
              (<= (l/width layout offset) target-width))
      layout)))
