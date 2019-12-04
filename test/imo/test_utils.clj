(ns imo.test-utils
  (:require [clojure.string :as string]
            [clojure.java.io :as io])
  (:import (com.github.difflib DiffUtils UnifiedDiffUtils)
           (java.util LinkedList)))

(defn load-test-file
  "Loads the contents of the given test file and returns
   it as a string."
  [filename]
  (let [f (io/file "test/__files__" filename)]
    (if (.exists f)
      (slurp f)
      (throw (IllegalArgumentException. ^String (str "Test file not found: " filename))))))

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
