(ns imo.test.preserve-style-tests
  (:require [clojure.test :refer :all]
            [imo.formatter.preserve-style :as preserve-style]
            [imo.test-utils :refer [load-test-file diff]]
            [imo.reader :as reader]))

(deftest clj-core-parsing-and-formatting-with-preserve-style
  (testing "output is same as input"
    (let [input (load-test-file "clojure_core.clj")
          output (-> (reader/read-ast input)
                     (preserve-style/format-node)
                     (pr-str))]
      (is (= "" (diff input output))))))
