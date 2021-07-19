(ns imo.formatter.meta-formatter
  (:require [imo.layout :as l]
            [imo.util :refer [begin-chars]]
            [imo.formatter.core :refer [format-node]]))

(defn format-meta-wrapper [[_ inner-node] offset target-width alternative ^String begin-chars]
  (when-let [inner (format-node inner-node (+ (.length begin-chars) offset) target-width alternative)]
    (l/create [begin-chars inner])))


(doseq [node-type [:meta :quote :syntax-quote :var-quote :unquote :unquote-splice :deref]
        :let [chars (begin-chars node-type)]]
  (assert chars (str "No begin chars for " node-type))
  (defmethod format-node node-type [node offset target-width alternative]
    (format-meta-wrapper node offset target-width alternative chars)))


(comment

  (fmt (analyze "(def ^:private n 123)"))


  -)
