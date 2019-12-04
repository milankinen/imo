(ns imo.formatter.context
  (:require [imo.formatter.clj-publics :as publics])
  (:import (java.util Map)))

(defrecord Binding [local fq-name name])

(def ^:private default-bindings
  (->> publics/clojure-core
       (map (fn [sym] [(symbol (name sym))
                       (->Binding false sym (symbol (name sym)))]))
       (into {})))

(defrecord Ctx
  [^Map options
   ^Integer level
   ^Map bindings
   ^Map aliases])

(defn ctx? [x]
  (instance? Ctx x))

(defn create
  "Creates a new context instance with sane defaults"
  [options]
  (let [level 0
        bindings default-bindings
        aliases {}]
    (->Ctx options
           level
           bindings
           aliases)))

(defn fq-name
  "Resolves fully qualified name of the given symbol or returns
   same symbol if fully qualified name is not available"
  [ctx sym]
  {:pre [(ctx? ctx)
         (symbol? sym)]}
  (or (get-in ctx [:bindings sym :fq-name])
      sym))
