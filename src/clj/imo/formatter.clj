(ns imo.formatter
  (:require [imo.formatter.preserve-style :as preserve]
            [imo.formatter.context :as context]))

(defn format-ast
  "Formats the given node and all its children with style
   that preserves the original formatting"
  [options node]
  (let [ctx (context/create options)]
    (preserve/format-node node)))
