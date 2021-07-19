(ns test-utils
  (:refer-clojure :exclude [format])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.walk :refer [postwalk]]
            [imo.util :refer [node?]]
            [imo.core :as imo]
            [imo.config :as config]))

(defn load-test-file
  "Loads the contents of the given test file and returns
   it as a string."
  [filename]
  (let [f (io/file "test/__files__" filename)]
    (if (.exists f)
      (slurp f)
      (throw (IllegalArgumentException. ^String (str "Test file not found: " filename))))))

(defn s
  "Creates source from the given lines"
  [& lines]
  (string/join "\n" lines))

(defn analyze
  "Reads and and analyzes an ast from the given source string using"
  ([s] (analyze s config/defaults))
  ([s config]
   {:pre [(string? s)]}
   (->> (string/split-lines s)
        (map #(string/replace % #"^\s*\|" ""))
        (string/join "\n")
        (imo/read)
        (imo/analyze config))))

(defn analyze*
  "Reads and analyzes an ast from the given forms using the default
   configuration"
  [& forms]
  (->> (vec (map pr-str forms))
       (string/join "\n")
       (analyze)))

(defn inspect
  "Reveals node's metadata as hiccup style 'props map' for
   inspection purposes"
  [ast {:keys [drop-keys]
        :or   {drop-keys []}
        :as   opts}]
  {:pre [(sequential? drop-keys)]}
  (postwalk
    #(if (node? %)
       (let [m (->> (apply dissoc (cons (inspect (meta %) opts) drop-keys))
                    (filter (comp some? second))
                    (into {}))]
         (into [(first %) m] (next %)))
       %)
    ast))
