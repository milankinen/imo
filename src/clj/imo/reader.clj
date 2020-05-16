(ns imo.reader
  (:require [imo.util :refer [node?]]
            [imo.logger :refer [timed]])
  (:import (imo SourceReader)))

;;
;;

(defn read-ast
  "Reads the CLJ(S) source string and returns AST in vector form
  [node-type & children]

  If the source can't be readed (it contains e.g. syntax errors),
  imo.ReaderException will be thrown."
  ([source] (read-ast source 2))
  ([source tab-size]
   {:pre  [(string? source)
           (pos-int? tab-size)]
    :post [(node? %)]}
   (timed "reader"
     (SourceReader/readAst source tab-size))))
