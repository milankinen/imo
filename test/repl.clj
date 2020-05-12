(ns repl
  (:require [imo.reader :as reader]
            [imo.logger :refer [timed] :as logger]
            [imo.test-utils :refer [load-test-file]]
            [clojure.string :as string]
            [clojure.walk :refer [postwalk]]
            [clojure.pprint :as pp]
            [imo.config :as config]))

(set! *warn-on-reflection* true)

(alter-var-root #'logger/*log-level* (constantly 5))

(defn ast [s]
  (->> (string/split-lines s)
       (map #(string/replace % #"^\s*\|" ""))
       (string/join "\n")
       (reader/read-ast)))

(defmacro ast* [& body]
  `(->> ~(vec (map str body))
        (string/join "\n")
        (ast)))

(def ^:dynamic *print-all* false)

(defn pr-ast [ast]
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
