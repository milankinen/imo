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
    (-main "--config-edn" "{:cache false}"
           "test/**/*.clj"
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

(defn pr-ast
  "Prints data and metadata from the given ast node"
  [ast]
  {:pre [(node? ast)]}
  ; pprint does not support meta so extract it and place
  ; it to the beginning of ast node
  (letfn [(process-meta [m]
            (if-not *print-all*
              (dissoc m :imo/node :pre :post :line :col)
              m))
          (lift-meta [x]
            (postwalk #(if (and (vector? %) (not (map-entry? %)))
                         (let [m (process-meta (or (lift-meta (meta %)) {}))]
                           (if (seq m)
                             (into [(first %) m] (rest %))
                             (into [(first %)] (rest %))))
                         %)
                      x))]
    (pp/pprint (lift-meta ast))))

(def clj-core
  (delay (load-test-file "clojure_core.clj")))
