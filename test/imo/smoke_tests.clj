(ns imo.smoke-tests
  (:require [clojure.test :refer :all]
            [test-utils :refer [load-test-file diff]]
            [imo.core :as imo]))

(deftest clj-core-parsing-and-formatting-with-preserve-style
  (testing "output is same as input"
    (let [input (load-test-file "clojure_core.clj")
          output (imo/format {:style :preserve} input)]
      (is (= "" (diff input output))))))
