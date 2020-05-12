(ns imo.analysis
  (:require [imo.logger :refer [timed]]
            [imo.util :refer [node?]]))

;;
;;

(defn analyze-ast
  "Runs static analysis to the given input ast and add annotates
   the returned ast nodes with analysis results"
  [ast]
  {:pre [(node? ast)
         (= :$ (first ast))]}
  (timed "analysis"
    ast))
