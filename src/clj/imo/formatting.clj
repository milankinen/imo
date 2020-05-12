(ns imo.formatting
  (:require [imo.logger :refer [timed]]
            [imo.util :refer [node->source]]))

;;
;;

(defn format-ast
  "Formats the given node and all its children"
  [_config ast]
  (timed "formatting"
    (node->source ast)))
