(ns imo.test-utils
  (:require [clojure.java.io :as io]))

(defn load-test-file
  "Loads the contents of the given test file and returns
   it as a string."
  [filename]
  (let [f (io/file "test/__files__" filename)]
    (if (.exists f)
      (slurp f)
      (throw (IllegalArgumentException. ^String (str "Test file not found: " filename))))))
