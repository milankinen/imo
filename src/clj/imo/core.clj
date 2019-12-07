(ns imo.core
  (:require [imo.reader :as reader]
            [imo.formatter :as formatter]
            [clojure.string :as string])
  (:import (java.util LinkedList)
           (com.github.difflib DiffUtils UnifiedDiffUtils)))

(defn format-source [config input]
  {:pre  [(string? input)
          (map? config)]
   :post [(string? %)]}
  (let [ast (reader/read-ast input)
        blocks (formatter/format-ast config ast)
        output (pr-str blocks)]
    output))

(defn diff
  "Returns a string diff from then given expected and actual
   contents in unified patch format.

   If contents are equal, empty string is returned.
   "
  [expected actual]
  {:pre [(string? expected)
         (string? actual)]}
  (let [expected-lines (LinkedList. (string/split-lines expected))
        actual-lines (LinkedList. (string/split-lines actual))
        patch (DiffUtils/diff expected-lines actual-lines)
        udiff (UnifiedDiffUtils/generateUnifiedDiff "expected" "actual" expected-lines patch 1)]
    (with-out-str
      (doseq [d udiff]
        (println d)))))
