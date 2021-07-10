(ns repl
  (:require [imo.main :refer [-main *exit-jvm*]]
            [imo.logger :refer [timed] :as logger]
            [test-utils :refer [load-test-file analyze*]]
            [imo.util :refer [node?]]
            [clojure.walk :refer [postwalk]]
            [clojure.pprint :as pp]
            [imo.config :as config]))

(alter-var-root #'logger/*log-level* (constantly 5))

(defn format-project!
  "Formats the entire project files"
  []
  (binding [*exit-jvm* false]
    (-main "test/**/*.clj"
           "src/**/*.clj"
           "!test/__files__/*")))

(def ^:dynamic *print-all*
  "Set to `true` to print **all** ast data with `pr-ast`"
  false)

(defn explain
  "Explains the analyzed ast node"
  [node]
  {:pre [(node? node)]}
  (letfn [(walk [value]
            (postwalk (fn [x]
                        (if (node? x)
                          (let [m (meta x)
                                m (if-not *print-all*
                                    (dissoc m :imo/node :pre :post :line :col)
                                    m)
                                m (walk m)]
                            (if (seq m)
                              (into [(first x) m] (rest x))
                              (into [(first x)] (rest x))))
                          x))
                      value))]
    (walk node)))

(defn explain*
  "Explains the the given form after the form has been analyzed"
  [& body]
  (explain (apply analyze* body)))

(defn pr-ast
  "Prints explained data (and metadata) from the given ast node"
  [ast]
  {:pre [(node? ast)]}
  (pp/pprint (explain ast)))

(def clj-core
  (delay (load-test-file "clojure_core.clj")))
