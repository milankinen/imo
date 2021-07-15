(ns imo.ignore-tests
  (:require [clojure.test :refer :all]
            [test-utils :refer [src analyze inspect]]))

(deftest ignore-analysis
  (testing "#_:imo/ignore marks next node as ignored"
    (let [[_ [_ _ & nodes]]
          (analyze (src "|(do 1
                         |    #_:imo/ignore
                         |    [2]
                         |    3)"))]
      (is (= '[[:number {:post ([:newline {} "\n"])
                         :pre  ([:space {} " "])}
                "1"]
               [:vector {:ignore? true
                         :post    ([:newline {} "\n"])
                         :pre     ([:space {} "    "]
                                   [:discard {}
                                    [:keyword {} ":imo/ignore"]]
                                   [:newline {} "\n"]
                                   [:space {} "    "])}
                [:number {} "2"]]
               [:number {:pre ([:space {} "    "])}
                "3"]]
             (inspect nodes {:drop-keys [:imo/node :line :col :comments]})))))
  (testing "ignore works for other meta nodes as well"
    (let [[_ [_ _ & nodes]]
          (analyze (src "|(do 1
                         |    #_:imo/ignore
                         |    #_(foo 2)
                         |    3)"))]
      (is (= '[[:number
                {:post ([:newline {} "\n"])
                 :pre  ([:space {} " "])}
                "1"]
               [:number
                {:pre ([:space {} "    "]
                       [:discard {}
                        [:keyword {} ":imo/ignore"]]
                       [:newline {} "\n"]
                       [:space {} "    "]
                       [:discard {:ignore? true}
                        [:list {:invocation foo
                                :resolve-as foo}
                         [:symbol {} "foo"]
                         [:number {:pre ([:space {} " "])}
                          "2"]]]
                       [:newline {} "\n"]
                       [:space {} "    "])}
                "3"]]
             (inspect nodes {:drop-keys [:imo/node :line :col :comments]}))))
    (let [[_ [_ _ & nodes]]
          (analyze (src "|(do 1
                         |    #_:imo/ignore
                         |    ^:foo
                         |    3)"))]
      (is (= '[[:number
                {:post ([:newline {} "\n"])
                 :pre  ([:space {} " "])}
                "1"]
               [:number {:pre ([:space {} "    "]
                               [:discard {}
                                [:keyword {} ":imo/ignore"]]
                               [:newline {} "\n"]
                               [:space {} "    "]
                               [:meta {:ignore? true}
                                [:keyword {} ":foo"]]
                               [:newline {} "\n"]
                               [:space {} "    "])}
                "3"]]
             (inspect nodes {:drop-keys [:imo/node :line :col :comments]}))))))
