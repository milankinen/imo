(ns imo.ignore-tests
  (:require [clojure.test :refer :all]
            [test-utils :refer [s analyze inspect]]))

(def non-relevant-meta-keys
  [:imo/node :line :col :inner-length :outer-length :inner-lines :outer-lines :pre* :post*])

(deftest ignore-analysis
  (testing "#_:imo/ignore marks next node as ignored"
    (let [[_ [_ _ & nodes]]
          (analyze (s "(do 1"
                      "    #_:imo/ignore"
                      "    [2]"
                      "    3)"))]
      (is (= '[[:number {} "1"]
               [:vector {:ignore? true
                         :pre     ([:discard {}
                                    [:keyword {} ":imo/ignore"]])}
                [:number {} "2"]]
               [:number {}
                "3"]]
             (inspect nodes {:drop-keys non-relevant-meta-keys})))))
  (testing "ignore works for other meta nodes as well"
    (let [[_ [_ _ & nodes]]
          (analyze (s "(do 1"
                      "    #_:imo/ignore"
                      "    #_(foo 2)"
                      "    3)"))]
      (is (= '[[:number {} "1"]
               [:number
                {:pre ([:discard {}
                        [:keyword {} ":imo/ignore"]]
                       [:discard {:ignore? true}
                        [:list {:invocation foo
                                :resolve-as foo}
                         [:symbol {} "foo"]
                         [:number {} "2"]]])}
                "3"]]
             (inspect nodes {:drop-keys non-relevant-meta-keys}))))
    (let [[_ [_ _ & nodes]]
          (analyze (s "(do 1"
                      "    #_:imo/ignore"
                      "    ^:foo"
                      "    3)"))]
      (is (= '[[:number {} "1"]
               [:number {:pre ([:discard {}
                                [:keyword {} ":imo/ignore"]]
                               [:meta {:ignore? true}
                                [:keyword {} ":foo"]])}
                "3"]]
             (inspect nodes {:drop-keys non-relevant-meta-keys}))))))
