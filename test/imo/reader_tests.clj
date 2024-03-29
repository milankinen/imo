(ns imo.reader-tests
  (:require [clojure.test :refer :all]
            [test-utils :refer [s inspect]]
            [imo.core :as imo])
  (:import (imo ImoException)))

(defn- read* [& lines]
  (imo/read (apply s lines)))

(def ^:private line-col {:drop-keys [:imo/node :inner-length :outer-length :inner-lines :outer-lines]})
(def ^:private meta-nodes-only {:drop-keys [:imo/node :line :col :inner-length :outer-length :inner-lines :outer-lines]})
(def ^:private stats-only {:drop-keys [:imo/node :line :col]})

(deftest leaf-nodes-reading
  (testing "all clojure primitives are supported"
    (is (= [:$
            [:number "123"]
            [:number "0xDEADBEEF"]
            [:nil "nil"]
            [:boolean "true"]
            [:boolean "false"]
            [:string "\"tsers\""]
            [:symbol "lol"]
            [:keyword ":bal"]
            [:keyword ":lol/bal"]
            [:keyword "::tsers"]
            [:char "\\space"]]
           (read* "123"
                  "0xDEADBEEF"
                  "nil"
                  "true"
                  "false"
                  "\"tsers\""
                  "lol"
                  ":bal"
                  ":lol/bal"
                  "::tsers"
                  "\\space")))))

(deftest whitespace-reading
  (testing "all whitespaces, comments and discards prior to node are marked as :pre"
    (is (= '[:$ {}
             [:symbol {:pre ([:comment {} "; this is a comment"]
                             [:newline {} "\n"]
                             [:space {} "  "]
                             [:discard {}
                              [:symbol {:pre ([:space {} "  "])} "foobar"]]
                             [:space {} " "]
                             [:discard {} [:symbol {} "bar"]]
                             [:newline {} "\n"])}
              "foo"]]
           (-> (read* "; this is a comment"
                      "  #_  foobar #_bar"
                      "foo")
               (inspect meta-nodes-only)))))
  (testing "all whitespaces and discards after node but before first newline are marked as :post"
    (is (= '[:$ {}
             [:symbol {:post ([:space {} " "]
                              [:discard {} [:symbol {} "lol"]]
                              [:space {} " "]
                              [:discard {} [:symbol {} "bal"]]
                              [:newline {} "\n"])}
              "foo"]
             [:symbol {} "bar"]]
           (-> (read* "foo #_lol #_bal"
                      "bar")
               (inspect meta-nodes-only)))))
  (testing "whitespace is preseved as it is"
    (is (= '[:$ {}
             [:symbol {} "foo"]
             [:symbol {:pre ([:space {} ",  "])}
              "bar"]]
           (-> (read* "foo,  bar")
               (inspect meta-nodes-only)))))
  (testing "discards are treated like any other whitespace"
    (is (= '[:$ {}
             [:symbol {:pre ([:space {} " "]
                             [:comment {} "; test"]
                             [:newline {} "\n"]
                             [:discard {} [:symbol {} "foo"]]
                             [:newline {} "\n"])}
              "bar"]]
           (-> (read* " ; test"
                      "#_foo"
                      "bar")
               (inspect meta-nodes-only))))
    (is (= '[:$ {}
             [:symbol {:post ([:space {} " "]
                              [:discard {} [:symbol {} "lol"]]
                              [:space {} " "]
                              [:comment {} ";bal"])}
              "bar"]]
           (-> (read* "bar #_lol ;bal")
               (inspect meta-nodes-only))))))

(deftest line-col-annotations
  (testing "line and column info is added to all read nodes"
    (is (= '[:$ {:col  1
                 :line 1}
             [:symbol {:col  1
                       :line 3
                       :pre  ([:space {:col  1
                                       :line 1}
                               " "]
                              [:comment {:col  2
                                         :line 1}
                               "; test"]
                              [:newline {:col  8
                                         :line 1}
                               "\n"]
                              [:discard {:col  1
                                         :line 2}
                               [:symbol {:col  3
                                         :line 2}
                                "foo"]]
                              [:newline {:col  6
                                         :line 2}
                               "\n"])}
              "bar"]]
           (-> (read* " ; test"
                      "#_foo"
                      "bar")
               (inspect line-col)))))
  (testing "tab size is configurable"
    (doseq [tab-size [2]]
      (is (= [:$ {:col  1
                  :line 1}
              [:symbol {:col  (inc tab-size)
                        :line 1
                        :pre  '([:space {:col  1
                                         :line 1}
                                 "\t"])}
               "bar"]]
             (inspect (imo/read "\tbar" tab-size) line-col))))))

(deftest collection-nodes-reading
  (testing "all clojure collections are supported"
    (is (= [:$
            [:list [:number "1"] [:number "2"]]
            [:list]
            [:vector [:number "1"] [:number "2"]]
            [:vector]
            [:set [:number "1"] [:number "2"]]
            [:set]
            [:map [:keyword ":lol"] [:number "1"] [:keyword ":bal"] [:number "2"]]
            [:map]
            [:ns-map [:keyword ":foo"] [:map [:keyword ":bar"] [:nil "nil"]]]
            [:ns-map [:keyword "::"] [:map [:keyword ":bar"] [:nil "nil"]]]
            [:ns-map [:keyword "::foo"] [:map [:keyword ":bar"] [:nil "nil"]]]]
           (read* "(1 2) ()"
                  "[1 2] []"
                  "#{1 2} #{}"
                  "{:lol 1 :bal 2} {}"
                  "#:foo {:bar nil}"
                  "#:: {:bar nil}"
                  "#::foo {:bar nil}"))))
  (testing "leading item whitespace is marked correctly"
    (is (= '[:vector {} [:symbol {:pre ([:space {} "  "])} "foo"]]
           (-> (read* "[  foo]") (inspect meta-nodes-only) (last))))
    (is (= '[:vector {}
             [:symbol {:pre ([:space {} " "]
                             [:newline {} "\n"]
                             [:space {} " "])}
              "foo"]]
           (-> (read* "[ \n foo]") (inspect meta-nodes-only) (last)))))
  (testing "trailing item whitespace is marked correctly"
    ; Trailing whitespace
    (is (= '[:vector {} [:symbol {:post ([:space {} "   "])} "foo"]]
           (-> (read* "[foo   ]") (inspect meta-nodes-only) (last)))))
  (testing "floating whitespace (after last item's newline) is marked as :children"
    (is (= '[:vector {:children ([:space {} "  "])}
             [:symbol {:post ([:space {} " "]
                              [:newline {} "\n"])}
              "foo"]]
           (-> (read* "[foo \n  ]") (inspect meta-nodes-only) (last)))))
  (testing "empty collection whitespace is marked as :children"
    (is (= '[:vector {:children ([:space {} "   "])}]
           (-> (read* "[   ]") (inspect meta-nodes-only) (last))))
    (is (= '[:vector {:children ([:space {} " "]
                                 [:newline {} "\n"]
                                 [:space {} " "])}]
           (-> (read* "[ \n ]") (inspect meta-nodes-only) (last))))))

(deftest macros-reading
  (testing "all basic macros are supported"
    (is (= [:$
            [:deref [:symbol "val"]]
            [:quote [:symbol "quoted"]]
            [:syntax-quote [:symbol "quoted"]]
            [:var-quote [:symbol "var"]]
            [:unquote [:symbol "x"]]
            [:unquote-splice [:symbol "xs"]]]
           (read* "@val"
                  "'quoted"
                  "`quoted"
                  "#'var"
                  "~x"
                  "~@xs"))))
  (testing "all dispatch macros are supported"
    (is (= [:$
            [:tagged-literal [:symbol "tagged"] [:string "\"literal\""]]
            [:anon-fn [:symbol "lol"] [:symbol "%"]]
            [:reader-cond [:list [:keyword ":clj"] [:number "123"]]]
            [:reader-cond-splice
             [:list [:keyword ":clj"] [:vector [:number "1"] [:number "2"]]]]
            [:symbolic-val [:symbol "Inf"]]]
           (read* "#tagged \"literal\""
                  "#(lol %)"
                  "#?(:clj 123)"
                  "#?@(:clj [1 2])"
                  "##Inf")))))

(deftest metadata-reading
  (testing "symbol, keyword and map metadatas are supported"
    (is (= '[:$ {}
             [:vector
              {:pre ([:meta {} [:symbol {} "List"]]
                     [:space {} " "]
                     [:meta {} [:keyword {} ":private"]]
                     [:space {} " "]
                     [:meta {}
                      [:map {}
                       [:keyword {} ":doc"]
                       [:symbol {:pre ([:space {} " "])} "..."]]]
                     [:space {} " "])}]]
           (-> (read* "^List ^:private ^{:doc ...} []")
               (inspect meta-nodes-only)))))
  (testing "line breaks between metadata nodes are working"
    (is (= '[:$ {}
             [:vector
              {:pre ([:meta {} [:symbol {} "Foo"]]
                     [:newline {} "\n"]
                     [:meta {} [:symbol {} "Bal"]]
                     [:space {} " "])}]]
           (-> (read* "^Foo"
                      "^Bal []")
               (inspect meta-nodes-only)))))
  (testing "special case: next node's meta in previous node's :post position"
    (is (= '[:$ {}
             [:symbol {:post ([:space {} " "])}
              "foo"]
             [:symbol {:pre ([:meta {}
                              [:symbol {} "Lol"]]
                             [:newline {} "\n"]
                             [:space {} " "])}
              "bal"]]
           (-> (read* "foo ^Lol"
                      " bal")
               (inspect meta-nodes-only))))))

(deftest node-stats-reading
  (testing "nodes contain information about their minimun length and required linebreaks"
    (is (= '[:$
             {:inner-length 38
              :inner-lines  3
              :outer-length 38
              :outer-lines  3}
             [:list
              {:inner-length 38
               :inner-lines  2
               :outer-length 38
               :outer-lines  3
               :pre          ([:comment
                               {:inner-length 0
                                :inner-lines  1
                                :outer-length 0
                                :outer-lines  1}
                               "; declare foo function"]
                              [:newline
                               {:inner-length 0
                                :inner-lines  0
                                :outer-length 0
                                :outer-lines  0}
                               "\n"])}
              [:symbol
               {:inner-length 4
                :inner-lines  0
                :outer-length 4
                :outer-lines  0}
               "defn"]
              [:symbol
               {:inner-length 3
                :inner-lines  0
                :outer-length 3
                :outer-lines  0
                :pre          ([:space
                                {:inner-length 0
                                 :inner-lines  0
                                 :outer-length 0
                                 :outer-lines  0}
                                " "])}
               "foo"]
              [:vector
               {:inner-length 15
                :inner-lines  1
                :outer-length 15
                :outer-lines  1
                :post         ([:newline
                                {:inner-length 0
                                 :inner-lines  0
                                 :outer-length 0
                                 :outer-lines  0}
                                "\n"])
                :pre          ([:space
                                {:inner-length 0
                                 :inner-lines  0
                                 :outer-length 0
                                 :outer-lines  0}
                                " "])}
               [:symbol
                {:inner-length 3
                 :inner-lines  0
                 :outer-length 3
                 :outer-lines  1
                 :post         ([:space
                                 {:inner-length 0
                                  :inner-lines  0
                                  :outer-length 0
                                  :outer-lines  0}
                                 " "]
                                [:comment
                                 {:inner-length 0
                                  :inner-lines  1
                                  :outer-length 0
                                  :outer-lines  1}
                                 "; first arg"]
                                [:newline
                                 {:inner-length 0
                                  :inner-lines  0
                                  :outer-length 0
                                  :outer-lines  0}
                                 "\n"])}
                "bar"]
               [:symbol
                {:inner-length 3
                 :inner-lines  0
                 :outer-length 9
                 :outer-lines  0
                 :pre          ([:space
                                 {:inner-length 0
                                  :inner-lines  0
                                  :outer-length 0
                                  :outer-lines  0}
                                 "           "]
                                [:meta
                                 {:inner-length 5
                                  :inner-lines  0
                                  :outer-length 5
                                  :outer-lines  0}
                                 [:symbol
                                  {:inner-length 4
                                   :inner-lines  0
                                   :outer-length 4
                                   :outer-lines  0}
                                  "long"]]
                                [:space
                                 {:inner-length 0
                                  :inner-lines  0
                                  :outer-length 0
                                  :outer-lines  0}
                                 " "])}
                "baz"]]
              [:list
               {:inner-length 11
                :inner-lines  0
                :outer-length 11
                :outer-lines  1
                :pre          ([:space
                                {:inner-length 0
                                 :inner-lines  0
                                 :outer-length 0
                                 :outer-lines  0}
                                "  "]
                               [:comment
                                {:inner-length 0
                                 :inner-lines  1
                                 :outer-length 0
                                 :outer-lines  1}
                                "; subtract bar from baz"]
                               [:newline
                                {:inner-length 0
                                 :inner-lines  0
                                 :outer-length 0
                                 :outer-lines  0}
                                "\n"]
                               [:space
                                {:inner-length 0
                                 :inner-lines  0
                                 :outer-length 0
                                 :outer-lines  0}
                                "  "])}
               [:symbol
                {:inner-length 1
                 :inner-lines  0
                 :outer-length 1
                 :outer-lines  0}
                "-"]
               [:symbol
                {:inner-length 3
                 :inner-lines  0
                 :outer-length 3
                 :outer-lines  0
                 :pre          ([:space
                                 {:inner-length 0
                                  :inner-lines  0
                                  :outer-length 0
                                  :outer-lines  0}
                                 " "])}
                "baz"]
               [:symbol
                {:inner-length 3
                 :inner-lines  0
                 :outer-length 3
                 :outer-lines  0
                 :pre          ([:space
                                 {:inner-length 0
                                  :inner-lines  0
                                  :outer-length 0
                                  :outer-lines  0}
                                 " "])}
                "bar"]]]]
           (-> (read* "; declare foo function"
                      "(defn foo [bar ; first arg"
                      "           ^long baz]"
                      "  ; subtract bar from baz"
                      "  (- baz bar))")
               (inspect stats-only))))))

(deftest discard+meta-reading
  (testing "discarding is applied to the node after meta nodes"
    (is (= '[:$ {:post ([:discard {}
                         [:number {:pre ([:meta {}
                                          [:keyword {} ":foo"]]
                                         [:space {} " "]
                                         [:meta {}
                                          [:keyword {} ":bar"]]
                                         [:space {} " "])}
                          "123"]])}]
           (-> (read* "#_^:foo ^:bar 123")
               (inspect meta-nodes-only))))))

(deftest meta+discard-reading
  (testing "discarding is applied to the node after meta nodes"
    (is (= '[:$ {}
             [:number {:pre ([:meta {}
                              [:keyword {} ":foo"]]
                             [:space {} " "]
                             [:discard {}
                              [:number {} "1"]]
                             [:space {} " "])}
              "2"]]
           (-> (read* "^:foo #_1 2")
               (inspect meta-nodes-only))))))

(deftest nested-discard-reading
  (testing "discards can be nested"
    (is (= '[:$ {}
             [:number {:pre ([:discard {}
                              [:number {:pre ([:discard {} [:number {} "1"]]
                                              [:space {} " "])}
                               "2"]]
                             [:space {} " "])}
              "3"]]
           (-> (read* "#_#_1 2 3")
               (inspect meta-nodes-only))))))

(deftest danling-metadata-nodes-reading
  (testing "dangling metadata nodes before end of collection should throw an exception"
    (is (thrown? ImoException "Unmatching paren ')'"
                 (read* "(foo ^:bar \n)"))))
  (testing "dangling metadata nodes before end of file should throw an exception"
    (is (thrown? ImoException "EOF while reading"
                 (read* "foo ^:bar")))))
