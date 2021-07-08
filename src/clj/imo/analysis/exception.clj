(ns imo.analysis.exception
  (:require [imo.util :refer [node? start-of end-of]])
  (:import (imo AnalysisException)))

(defn analysis-ex [message causing-node]
  {:pre [(string? message)
         (or (node? causing-node)
             (nil? causing-node))]}
  (AnalysisException. message causing-node))

(defn ex-position [^AnalysisException ex parent-node]
  {:pre [(instance? AnalysisException ex)
         (node? parent-node)]}
  (if-let [caused-by (.-causingNode ex)]
    (start-of caused-by)
    (end-of parent-node)))
