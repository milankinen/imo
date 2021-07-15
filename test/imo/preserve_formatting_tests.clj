(ns imo.preserve-formatting-tests
  (:require [clojure.test :refer :all]
            [test-utils :refer [src analyze empty-test-layout]]
            [imo.layout.core :as l]))

(deftest column-offset-shifting
  (testing "shifting preserved node left"
    (let [[_ [_ _ _ map-node]]
          (analyze (src "|(def foo {:a 1
                         |             :b 2})"))]
      (is (= (src "|(def foo
                   |  {:a 1
                   |      :b 2})")
             (-> (empty-test-layout)
                 (l/add-token "(def foo")
                 (l/add-line-break)
                 (l/add-token "  ")
                 (l/format-preserve map-node)
                 (l/add-token ")")
                 (l/layout->source))))))
  (testing "shifting preserved node right"
    (let [[_ [_ _ _ map-node]]
          (analyze (src "|(def foo {:a 1
                       |             :b 2})"))]
      (is (= (src "|(def foobar {:a 1
                   |                :b 2})")
             (-> (empty-test-layout)
                 (l/add-token "(def foobar ")
                 (l/format-preserve map-node)
                 (l/add-token ")")
                 (l/layout->source))))))
  (testing "multiline strings are not shifted"
    (let [[_ [_ _ _ vec-node]]
          (analyze (src "|(def foo [1 \"multi
                         |              line\" 2
                         |          3])"))]
      (is (= (src "|(def foobar [1 \"multi
                   |              line\" 2
                   |             3])")
             (-> (empty-test-layout)
                 (l/add-token "(def foobar ")
                 (l/format-preserve vec-node)
                 (l/add-token ")")
                 (l/layout->source))))
      (is (= (src "|(def foo
                   |  [1 \"multi
                   |              line\" 2
                   |   3])")
             (-> (empty-test-layout)
                 (l/add-token "(def foo")
                 (l/add-line-break)
                 (l/add-token "  ")
                 (l/format-preserve vec-node)
                 (l/add-token ")")
                 (l/layout->source)))))))
