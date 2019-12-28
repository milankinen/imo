(ns imo.config
  (:require [schema.core :as s]))

(def PosInt
  (s/constrained s/Int pos-int? 'PosInt))

(def Config
  {:width            PosInt
   :optimize-imports s/Bool})

(def defaults
  {:width            80
   :optimize-imports false})

(defn check
  "Checks whether the given config is valid or not. If config is not
   valid, returns a string of errors."
  [config]
  (when-let [errs (s/check Config config)]
    (with-out-str (print errs))))
