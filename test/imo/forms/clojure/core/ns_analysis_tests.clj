(ns imo.forms.clojure.core.ns-analysis-tests
  (:require [clojure.test :refer :all]
            [test-utils :refer [analyze*]]
            [clojure.walk :as walk]))

(defn- get-invocation [node]
  (:invocation (meta node)))

(defn- invalid? [node]
  (let [status (volatile! false)]
    (walk/postwalk #(do (when (:invalid? (meta %))
                          (vreset! status true))
                        %)
                   node)
    @status))

(deftest refer-clojure-analysis
  (testing "default core exports"
    (let [[_ inc-node dec-node]
          (analyze*
            '(inc 1)
            '(dec))]
      (is (= 'clojure.core/inc (get-invocation inc-node)))
      (is (= 'clojure.core/dec (get-invocation dec-node)))))
  (testing "default java imports"
    (let [[_ value-of-node ctor-node]
          (analyze*
            '(Integer/valueOf 1)
            '(Integer. 1))]
      (is (= 'java.lang.Integer/valueOf (get-invocation value-of-node)))
      (is (= 'java.lang.Integer. (get-invocation ctor-node)))))
  (testing ":exclude"
    (let [[_ _ inc-node dec-node]
          (analyze*
            '(ns test-ns
               (:refer-clojure :exclude [dec]))
            '(inc 1)
            '(dec 1))]
      (is (= 'clojure.core/inc (get-invocation inc-node)))
      (is (= 'dec (get-invocation dec-node)))))
  (testing ":only"
    (let [[_ _ inc-node dec-node]
          (analyze*
            '(ns test-ns
               (:refer-clojure :only [dec]))
            '(inc 1)
            '(dec 1))]
      (is (= 'inc (get-invocation inc-node)))
      (is (= 'clojure.core/dec (get-invocation dec-node)))))
  (testing ":rename"
    (let [[_ _ dec-node mydec-node]
          (analyze*
            '(ns test-ns
               (:refer-clojure :rename {dec mydec}))
            '(dec 1)
            '(mydec 1))]
      (is (= 'dec (get-invocation dec-node)))
      (is (= 'clojure.core/dec (get-invocation mydec-node))))))

(deftest require-analysis
  (testing "libspec forms"
    (let [ast (analyze*
                '(ns test-ns
                   (:require foo.bar
                             [foo.bar]
                             [foo.bar :refer [a] :rename {a b} :reload]
                             [clojure.test :refer :all :verbose :reload-all]
                             (foo [bar]
                                  baz))))]
      (is (not (invalid? ast)))))
  (testing ":refer [libs...]"
    (let [[_ _ foo-node bar-node join-node split-node]
          (analyze*
            '(ns test-ns
               (:require [mylib :refer [foo bar]]
                         [clojure.string :refer [join]]))
            '(foo)
            '(bar)
            '(join)
            '(split))]
      (is (= 'mylib/foo (get-invocation foo-node)))
      (is (= 'mylib/bar (get-invocation bar-node)))
      (is (= 'clojure.string/join (get-invocation join-node)))
      (is (= 'split (get-invocation split-node)))))
  (testing ":refer :all"
    (let [[_ _ join-node split-node]
          (analyze*
            '(ns test-ns
               (:require [clojure.string :refer :all]))
            '(join)
            '(split))]
      (is (= 'clojure.string/join (get-invocation join-node)))
      (is (= 'clojure.string/split (get-invocation split-node)))))
  (testing ":refer ... :rename"
    (let [[_ _ join-node union-node]
          (analyze*
            '(ns test-ns
               (:require [clojure.string :refer :all :rename {join s-join}]
                         [clojure.set :refer [union] :rename {union set-union}]))
            '(s-join)
            '(set-union))]
      (is (= 'clojure.string/join (get-invocation join-node)))
      (is (= 'clojure.set/union (get-invocation union-node))))))

(deftest import-analysis
  (testing "import forms"
    (let [ast (analyze*
                '(ns test-ns
                   (:import java.util.regex.Pattern
                            [java.util.regex Pattern Matcher]
                            (java.util.regex Pattern Matcher))))]
      (is (not (invalid? ast)))))
  (testing "single class import"
    (let [[_ _ compile-node ctor-node]
          (analyze*
            '(ns test-ns
               (:import java.util.regex.Pattern))
            '(Pattern/compile "")
            '(Pattern.))]
      (is (= 'java.util.regex.Pattern/compile (get-invocation compile-node)))
      (is (= 'java.util.regex.Pattern. (get-invocation ctor-node)))))
  (testing "multi-class import"
    (let [[_ _ pattern-node matcher-node]
          (analyze*
            '(ns test-ns
               (:import (java.util.regex Pattern Matcher)))
            '(Pattern.)
            '(Matcher.))]
      (is (= 'java.util.regex.Pattern. (get-invocation pattern-node)))
      (is (= 'java.util.regex.Matcher. (get-invocation matcher-node))))))

(deftest use-analysis
  (testing "without :only"
    (let [[_ _ join-node split-node]
          (analyze*
            '(ns test-ns
               (:use [clojure.string]))
            '(join)
            '(split))]
      (is (= 'clojure.string/join (get-invocation join-node)))
      (is (= 'clojure.string/split (get-invocation split-node)))))
  (testing "with :only"
    (let [[_ _ join-node split-node]
          (analyze*
            '(ns test-ns
               (:use [clojure.string :only (split)]))
            '(join)
            '(split))]
      (is (= 'join (get-invocation join-node)))
      (is (= 'clojure.string/split (get-invocation split-node))))))

(deftest gen-class-analysis
  (testing "without options"
    (let [ast (analyze* '(ns test-ns
                           (:gen-class)))]
      (is (not (invalid? ast)))))
  (testing "with options"
    (let [ast (analyze* '(ns test-ns
                           (:gen-class
                             :name Test
                             :implements [java.lang.AutoCloseable])))]
      (is (not (invalid? ast))))))
