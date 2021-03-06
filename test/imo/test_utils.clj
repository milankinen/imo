(ns imo.test-utils
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.walk :refer [postwalk]]
            [imo.util :refer [node?]]))

(defn load-test-file
  "Loads the contents of the given test file and returns
   it as a string."
  [filename]
  (let [f (io/file "test/__files__" filename)]
    (if (.exists f)
      (slurp f)
      (throw (IllegalArgumentException. ^String (str "Test file not found: " filename))))))

(defn src
  "Creates source from string, stripping out left margin before |"
  [s]
  {:pre [(string? s)]}
  (string/replace s #"(^|\n)[ ]*\|" "$1"))

(defmacro src*
  "Creates source from the given forms, each form starting
   from new line"
  [& forms]
  (string/join "\n" (map str forms)))

(defn inspect
  "Reveals node's metadata as hiccup style 'props map' for
   inspection purposes"
  [ast {:keys [drop-keys]
        :or   {drop-keys []}
        :as   opts}]
  {:pre [(sequential? drop-keys)]}
  (postwalk
    #(if (node? %)
       (into [(first %) (apply dissoc (cons (inspect (meta %) opts) drop-keys))] (next %))
       %)
    ast))
