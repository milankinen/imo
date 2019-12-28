(ns imo.reader
  (:require [clojure.string :as string]
            [clojure.walk :refer [postwalk]]
            [clojure.pprint :as pp])
  (:import (imo SourceReader)))

(defn- normalize-newlines [s]
  (string/replace s #"(\r\n|\r)" "\n"))

(defn read-ast
  "Reads the CLJ(S) source string and returns AST in vector form
  [node-type & children]

  If the source can't be readed (it contains e.g. syntax errors),
  imo.ReaderException will be thrown.
  "
  [source]
  {:pre [(string? source)]}
  (-> (normalize-newlines source)
      (SourceReader/readAst)))

(comment

  (def ast (read-ast "\n"))

  (meta ast)

  '-)