(ns imo.test.smoke-tests
  (:require [clojure.test :refer :all]
            [imo.analysis :as analysis]
            [imo.test-utils :refer [load-test-file]]
            [imo.reader :as reader]
            [imo.util :refer [node->source]]))

(deftest reader-and-analysis-data-preserve-test
  (testing "reading and analysis do not drop any data from the read ast"
    (let [input (load-test-file "clojure_core.clj")
          output (-> (reader/read-ast input)
                     (analysis/analyze-ast)
                     (node->source))]
      ; If we can construct the input source back from analyzed ast,
      ; we know that we haven't dropped any relevant info
      (is (= input output)))))
