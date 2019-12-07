(ns imo.config
  (:require [schema.core :as s]))

(def PosInt
  (s/constrained s/Int pos-int? 'PosInt))

(def Width
  (s/constrained
    [(s/one PosInt "Target width")
     (s/one PosInt "Max width")]
    (fn [[target max]]
      (<= target max))
    "Target width must be smaller than max width"))

(def Config
  {:width            Width
   :optimize-imports s/Bool})

(def defaults
  {:width            [80 120]
   :optimize-imports false})

(defn check
  "Checks whether the given config is valid or not. If config is not
   valid, returns a string of errors."
  [config]
  (when-let [errs (s/check Config config)]
    (with-out-str (print errs))))
