(ns imo.test.preserve-style-tests
  (:require [clojure.test :refer :all]
            [imo.formatter.preserve :as preserve-style]
            [imo.test-utils :refer [load-test-file]]
            [imo.reader :as reader]
            [imo.core :refer [diff]]
            [imo.formatter.layout :as l]))

(deftest clj-core-parsing-and-formatting-with-preserve-style
  (testing "output is same as input"
    (let [input (load-test-file "clojure_core.clj")
          output (-> (reader/read-ast input)
                     (preserve-style/format-node (l/empty-output))
                     (l/to-src))]
      (is (= input output)))))
