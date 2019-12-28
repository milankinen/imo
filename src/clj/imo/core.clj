(ns imo.core
  (:require [imo.reader :as reader]
            [imo.formatter :as formatter]
            [imo.util :refer [split-lines]])
  (:import (java.util LinkedList)
           (com.github.difflib DiffUtils UnifiedDiffUtils)))

(defn format-source [config input]
  {:pre  [(string? input)
          (map? config)]
   :post [(string? %)]}
  (let [ast (reader/read-ast input)
        output (formatter/format-ast config ast)]
    output))

(defn diff
  "Returns a string diff from then given expected and actual
   contents in unified patch format.

   If contents are equal, empty string is returned.
   "
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
