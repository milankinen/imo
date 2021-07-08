(ns imo.core
  (:refer-clojure :exclude [read format])
  (:require [imo.analysis.core :refer [analyze-root]]
            [imo.layout :refer [format-root]]
            [imo.logger :refer [timed]]
            [imo.util :refer [node? split-lines]]
            [imo.forms])
  (:import (java.util LinkedList)
           (com.github.difflib DiffUtils UnifiedDiffUtils)
           (imo SourceReader)))

(defn read
  "Reads the CLJ(S) source string and returns AST in vector form
  [node-type & children]

  If the source can't be readed (it contains e.g. syntax errors),
  imo.ReaderException will be thrown."
  ([source] (read source 2))
  ([source tab-size]
   {:pre  [(string? source)
           (pos-int? tab-size)]
    :post [(node? %)]}
   (timed "reader"
     (SourceReader/readAst source tab-size))))

(defn analyze
  "Runs static analysis to the given input ast and add annotates the returned
   ast nodes with analysis results"
  [config ast]
  {:pre [(node? ast)
         (= :$ (first ast))]}
  (timed "analysis"
    (analyze-root (or (:resolve-as config) {}) ast)))

(defn format
  "Formats the given root ast node and returns the formatted source as string"
  [config ast]
  {:pre  [(node? ast)
          (= :$ (first ast))]
   :post [(string? %)]}
  (timed "format"
    (let [width (:width config)]
      (format-root width ast))))

(defn diff
  "Returns a string diff from then given expected and actual contents in unified
   patch format. If contents are equal, empty string is returned."
  [expected actual]
  {:pre [(string? expected)
         (string? actual)]}
  (let [expected-lines (LinkedList. (split-lines expected))
        actual-lines (LinkedList. (split-lines actual))
        patch (DiffUtils/diff expected-lines actual-lines)
        udiff (UnifiedDiffUtils/generateUnifiedDiff "expected" "actual" expected-lines patch 1)]
    (with-out-str
      (doseq [d udiff]
        (println d)))))
