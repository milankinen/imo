(ns repl
  (:require [imo.main :refer [-main *exit-jvm*]]
            [imo.logger :as logger]
            [test-utils :as utils]
            [imo.util :refer [node?]]
            [imo.config :as config]
            [imo.core :as imo]))

(logger/set-log-level! 5)

(defn format-project!
  "Formats the entire project files"
  []
  (binding [*exit-jvm* false]
    (-main "test/**/*.clj"
           "src/**/*.clj"
           "!test/__files__/*")))

(defn analyze [source]
  (utils/analyze source))

(defn analyze* [& forms]
  (apply utils/analyze* forms))

(def noise-keys
  [:imo/node
   :pre*
   :post*
   :children*
   :line
   :col
   :inner-lines
   :outer-lines
   :inner-length
   :outer-length])

(defn explain
  "Explains the analyzed ast node"
  ([node] (explain node false))
  ([node all?]
   {:pre [(node? node)]}
   (utils/inspect node {:drop-keys (if all? [] noise-keys)})))

(defn explain*
  "Explains the the given form after the form has been analyzed"
  [& body]
  (explain (apply utils/analyze* body) false))

(defn explain-all*
  "Explains the the given form after the form has been analyzed"
  [& body]
  (explain (apply utils/analyze* body) true))

(defn fmt [node]
  {:pre [(node? node)]}
  (let [root (if (not= :$ (first node))
               ^{:imo/node true} [:$ node]
               node)]
    (print "**** formatted ****" (str "\n" (imo/format config/defaults root)))))

(defn fmt* [& body]
  (->> (apply utils/analyze* body)
       (imo/format config/defaults)
       (str "\n")
       (print "**** formatted ****")))

(def clj-core
  (delay (utils/load-test-file "clojure_core.clj")))
