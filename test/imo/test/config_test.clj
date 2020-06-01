(ns imo.test.config-test
  (:require [clojure.test :refer :all]
            [clojure.data :refer [diff]]
            [imo.config :refer [defaults build-config]])
  (:import (imo ImoException)))

(deftest config-merging-test
  (testing "using only defaults produces a valid config"
    (is (= defaults (build-config nil nil))))
  (testing "config file overrides defaults"
    (let [c (build-config '{:width 111 :resolve-as {foo/bar def lol/bal if}} nil)]
      (is (= '{:resolve-as {foo/bar def
                            lol/bal if}
               :width      111}
             (second (diff defaults c))))))
  (testing "cli options overrides config file"
    (let [c (build-config '{:width 111 :resolve-as {foo/bar def lol/bal if}}
                          '{:width 112 :resolve-as {lol/bal when}})]
      (is (= '{:resolve-as {foo/bar def
                            lol/bal when}
               :width      112}
             (second (diff defaults c)))))))

(deftest symbol-resolution-building-test
  (testing "symbols get resolved until fixed point is reached"
    (is (= '{foo/bar def
             foo/baz lol/baz
             lol/bal def}
           (:resolve-as (build-config '{:resolve-as {foo/bar lol/bal
                                                     lol/bal def
                                                     foo/baz lol/baz}}
                                      nil)))))
  (testing "circular dependencies in resolution throw an exception"
    (is (thrown-with-msg?
          ImoException #"Circular dependency in :resolve-as map: lol/bal -> foo/baz -> foo/bar -> lol/bal"
          (build-config '{:resolve-as {foo/bar lol/bal
                                       lol/bal foo/baz
                                       foo/baz foo/bar}}
                        nil)))))
