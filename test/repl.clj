(ns repl
  (:require [imo.reader :as reader]
            [clojure.string :as string]))

(defn ast [s]
  (->> (string/split-lines s)
       (map #(string/replace % #"^\s*\|" ""))
       (string/join "\n")
       (reader/read-ast)))

(defn pr-ast [ast]
  (reader/print-ast ast))

(comment
  ; Do experiments here but do not commit ;-)


  '-)