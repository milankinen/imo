(ns imo.formatter.context
  (:import (java.util Map)))

(defrecord Ctx
  [^Map options
   ^Integer level
   ^Map bindings])

(defn create
  "Creates a new context instance with sane defaults"
  [options]
  (->Ctx options 0 {}))
