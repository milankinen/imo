(ns repl
  (:require [imo.main :refer [-main *exit-jvm*]]
            [imo.core :as imo]
            [imo.logger :refer [timed] :as logger]
            [imo.test-utils :refer [load-test-file]]
            [imo.util :refer [node?]]
            [clojure.string :as string]
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

(defn ast
  "Reads and and analyzes an ast from the given source string"
  [s]
  (->> (string/split-lines s)
       (map #(string/replace % #"^\s*\|" ""))
       (string/join "\n")
       (imo/read)
       (imo/analyze config/defaults)))

(defmacro ast*
  "Reads and analyzes an ast from the given forms"
  [& body]
  `(->> ~(vec (map str body))
        (string/join "\n")
        (ast)))

(def ^:dynamic *print-all*
  "Set to `true` to print **all** ast data with `pr-ast`"
  false)

(defn explain [node]
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

(defmacro explain* [& body]
  `(explain (ast* ~@body)))

(defn pr-ast
  "Prints explained data (and metadata) from the given ast node"
  [ast]
  {:pre [(node? ast)]}
  (pp/pprint (explain ast)))

(def clj-core
  (delay (load-test-file "clojure_core.clj")))
