(ns imo.reader-tests
  (:refer-clojure :exclude [read])
  (:require [clojure.test :refer :all]
            [test-utils :refer [src inspect]]
            [imo.core :as imo]))

(defn- read
  ([s] (read s {}))
  ([s {:keys [meta? tab-size] :or {meta? true tab-size 2} :as opts}]
   (as-> (imo/read (src s) tab-size) ast
         (if meta?
           (inspect ast opts)
           ast))))

(def ^:private line-col {:drop-keys [:imo/node :comments]})
(def ^:private no-line-col {:drop-keys [:imo/node :line :col :comments]})
(def ^:private comments-only {:drop-keys [:imo/node :line :col]})
(def ^:private no-meta {:meta? false})

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
           (read "| 123
                  | 0xDEADBEEF
                  | nil
                  | true
                  | false
                  | \"tsers\"
                  | lol
                  | :bal
                  | :lol/bal
                  | ::tsers
                  | \\space
                  |" no-meta)))))

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
           (read "|; this is a comment
                  |  #_  foobar #_bar
                  |foo" no-line-col))))
  (testing "all whitespaces and discards after node but before first newline are marked as :post"
    (is (= '[:$ {}
             [:symbol {:post ([:space {} " "]
                              [:discard {} [:symbol {} "lol"]]
                              [:space {} " "]
                              [:discard {} [:symbol {} "bal"]]
                              [:newline {} "\n"])}
              "foo"]
             [:symbol {} "bar"]]
           (read "|foo #_lol #_bal
                  |bar" no-line-col))))
  (testing "whitespace is preseved as it is"
    (is (= '[:$ {}
             [:symbol {} "foo"]
             [:symbol {:pre ([:space {} ",  "])}
              "bar"]]
           (read "foo,  bar" no-line-col))))
  (testing "discards are treated like any other whitespace"
    (is (= '[:$ {}
             [:symbol {:pre ([:space {} " "]
                             [:comment {} "; test"]
                             [:newline {} "\n"]
                             [:discard {} [:symbol {} "foo"]]
                             [:newline {} "\n"])}
              "bar"]]
           (read "| ; test
                  |#_foo
                  |bar" no-line-col)))
    (is (= '[:$ {}
             [:symbol {:post ([:space {} " "]
                              [:discard {} [:symbol {} "lol"]]
                              [:space {} " "]
                              [:comment {} ";bal"])}
              "bar"]]
           (read "|bar #_lol ;bal" no-line-col)))))

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
           (read "| ; test
                  |#_foo
                  |bar" line-col))))
  (testing "tab size is configurable"
    (doseq [tab-size [2 4]]
      (is (= [:$ {:col  1
                  :line 1}
              [:symbol {:col  (inc tab-size)
                        :line 1
                        :pre  '([:space {:col  1
                                         :line 1}
                                 "\t"])}
               "bar"]]
             (read "\tbar" (assoc line-col :tab-size tab-size)))))))

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
           (read "| (1 2) ()
                | [1 2] []
                | #{1 2} #{}
                | {:lol 1 :bal 2} {}
                | #:foo {:bar nil}
                | #:: {:bar nil}
                | #::foo {:bar nil}
                |" no-meta))))
  (testing "leading item whitespace is marked correctly"
    (is (= '[:vector {} [:symbol {:pre ([:space {} "  "])} "foo"]]
           (last (read "[  foo]" no-line-col))))
    (is (= '[:vector {}
             [:symbol {:pre ([:space {} " "]
                             [:newline {} "\n"]
                             [:space {} " "])}
              "foo"]]
           (last (read "[ \n foo]" no-line-col)))))
  (testing "trailing item whitespace is marked correctly"
    ; Trailing whitespace
    (is (= '[:vector {} [:symbol {:post ([:space {} "   "])} "foo"]]
           (last (read "[foo   ]" no-line-col)))))
  (testing "floating whitespace (after last item's newline) is marked as :hidden"
    (is (= '[:vector {:hidden ([:space {} "  "])}
             [:symbol {:post ([:space {} " "]
                              [:newline {} "\n"])}
              "foo"]]
           (last (read "[foo \n  ]" no-line-col)))))
  (testing "empty collection whitespace is marked as :hidden"
    (is (= '[:vector {:hidden ([:space {} "   "])}]
           (last (read "[   ]" no-line-col))))
    (is (= '[:vector {:hidden ([:space {} " "]
                               [:newline {} "\n"]
                               [:space {} " "])}]
           (last (read "[ \n ]" no-line-col))))))

(deftest macros-reading
  (testing "all basic macros are supported"
    (is (= [:$
            [:deref [:symbol "val"]]
            [:quote [:symbol "quoted"]]
            [:syntax-quote [:symbol "quoted"]]
            [:var-quote [:symbol "var"]]
            [:unquote [:symbol "x"]]
            [:unquote-splice [:symbol "xs"]]]
           (read "| @val
                  | 'quoted
                  | `quoted
                  | #'var
                  | ~x
                  | ~@xs
                  " no-meta))))
  (testing "all dispatch macros are supported"
    (is (= [:$
            [:tagged-literal [:symbol "tagged"] [:string "\"literal\""]]
            [:anon-fn [:symbol "lol"] [:symbol "%"]]
            [:reader-cond [:list [:keyword ":clj"] [:number "123"]]]
            [:reader-cond-splice
             [:list [:keyword ":clj"] [:vector [:number "1"] [:number "2"]]]]
            [:symbolic-val [:symbol "Inf"]]]
           (read "| #tagged \"literal\"
                  | #(lol %)
                  | #?(:clj 123)
                  | #?@(:clj [1 2])
                  | ##Inf
                  " no-meta)))))

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
           (read "^List ^:private ^{:doc ...} []"
                 no-line-col))))
  (testing "line breaks between metadata nodes are working"
    (is (= '[:$ {}
             [:vector
              {:pre ([:meta {} [:symbol {} "Foo"]]
                     [:newline {} "\n"]
                     [:meta {} [:symbol {} "Bal"]]
                     [:space {} " "])}]]
           (read "|^Foo
                  |^Bal []"
                 no-line-col))))
  (testing "special case: next node's meta in previous node's :post position"
    (is (= '[:$ {}
             [:symbol {:post ([:space {} " "])}
              "foo"]
             [:symbol {:pre ([:meta {}
                              [:symbol {} "Lol"]]
                             [:newline {} "\n"]
                             [:space {} " "])}
              "bal"]]
           (read "|foo ^Lol
                  | bal" no-line-col)))))

(deftest comments-reading
  (testing "comments are counted and summed inside node"
    (is (= '[:$ {:comments 3}
             [:list {:comments 3
                     :pre      ([:comment {:comments 1} "; declare foo function"]
                                [:newline {:comments 0} "\n"])}
              [:symbol {:comments 0} "defn"]
              [:symbol {:comments 0
                        :pre      ([:space {:comments 0} " "])}
               "foo"]
              [:vector {:comments 1
                        :post     ([:newline {:comments 0} "\n"])
                        :pre      ([:space {:comments 0} " "])}
               [:symbol {:comments 1
                         :post     ([:space {:comments 0} " "]
                                    [:comment {:comments 1} "; first arg"]
                                    [:newline {:comments 0} "\n"])}
                "bar"]
               [:symbol {:comments 0
                         :pre      ([:space {:comments 0} "           "])}
                "baz"]]
              [:list {:comments 1
                      :pre      ([:space {:comments 0} "  "]
                                 [:comment {:comments 1} "; subtract bar from baz"]
                                 [:newline {:comments 0} "\n"]
                                 [:space {:comments 0} "  "])}
               [:symbol {:comments 0}
                "-"]
               [:symbol {:comments 0
                         :pre      ([:space {:comments 0}
                                     " "])}
                "baz"]
               [:symbol {:comments 0
                         :pre      ([:space {:comments 0} " "])}
                "bar"]]]]
           (read "|; declare foo function
                  |(defn foo [bar ; first arg
                  |           baz]
                  |  ; subtract bar from baz
                  |  (- baz bar))"
                 comments-only)))))

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
           (read "#_^:foo ^:bar 123" no-line-col)))))

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
           (read "^:foo #_1 2" no-line-col)))))

(deftest nested-discard-reading
  (testing "discards can be nested"
    (is (= '[:$ {}
             [:number {:pre ([:discard {}
                              [:number {:pre ([:discard {} [:number {} "1"]]
                                              [:space {} " "])}
                               "2"]]
                             [:space {} " "])}
              "3"]]
           (read "#_#_1 2 3" no-line-col)))))
