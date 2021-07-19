(ns imo.preserve-format-tests
  (:require [clojure.test :refer :all]
            [test-utils :refer [s analyze]]
            [imo.formatter.core :refer [preserve-format]]
            [imo.layout :as l]))

(deftest column-offset-shifting
  (testing "shifting preserved node left"
    (let [[_ [_ _ _ map-node]]
          (analyze (s "(def foo {:a 1"
                      "             :b 2})"))]
      (is (= (s "(def foo"
                "  {:a 1"
                "      :b 2})")
             (-> ["(def foo" :break
                  "  " (preserve-format map-node) ")"]
                 (l/render))))))
  (testing "shifting preserved node right"
    (let [[_ [_ _ _ map-node]]
          (analyze (s "(def foo {:a 1"
                      "             :b 2})"))]
      (is (= (s "(def foobar {:a 1"
                "                :b 2})")
             (-> ["(def foobar " (preserve-format map-node) ")"]
                 (l/render))))))
  (testing "multiline strings are not shifted"
    (let [[_ [_ _ _ vec-node]]
          (analyze (s "(def foo [1 \"multi"
                      "               line\" 2"
                      "          3])"))]
      (is (= (s "(def foobar [1 \"multi"
                "               line\" 2"
                "             3])")
             (-> ["(def foobar " (preserve-format vec-node) ")"]
                 (l/render))))
      (is (= (s "(def foobar"
                "  [1 \"multi"
                "               line\" 2"
                "   3])")
             (-> (-> ["(def foobar" :break
                      "  " (preserve-format vec-node) ")"]
                     (l/render))
                 (l/render)))))))
