(ns imo.test.preprocessing-tests
  (:require [clojure.test :refer :all]
            [imo.analysis :as analysis]
            [imo.test-utils :refer [load-test-file]]
            [imo.reader :as reader]
            [imo.util :refer [node->source]]))

(deftest clj-core-parsing-and-analysis-test
  (testing "parsing and analysis preserve ast so that ast can be rendered back to the source"
    (let [input (load-test-file "clojure_core.clj")
          output (-> (reader/read-ast input)
                     (analysis/analyze-ast)
                     (node->source))]
      (is (= input output)))))
