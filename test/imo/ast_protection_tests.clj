(ns imo.ast-protection-tests
  (:require [clojure.test :refer :all]
            [test-utils :refer [load-test-file]]
            [imo.core :as imo]
            [imo.util :refer [node->source]]
            [imo.config :as config]))

(deftest reader-and-analysis-data-protection
  (testing "reading and analysis do not drop any data from the read ast"
    (let [input (load-test-file "clojure_core.clj")
          output (->> (imo/read input)
                      (imo/analyze config/defaults)
                      (node->source))]
      ; If we can construct the input source back from analyzed ast,
      ; we know that we haven't dropped any relevant info
      (is (= input output)))))
