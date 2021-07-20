(ns imo.formatter.meta-formatter
  (:require [imo.layout.core :as l]
            [imo.util :refer [begin-chars]]
            [imo.formatter.core :refer [format-inner-node]]))

(doseq [node-type [:meta :quote :syntax-quote :var-quote :unquote :unquote-splice :deref]
        :let [chars (begin-chars node-type)]]
  (assert chars (str "No begin chars for " node-type))
  (defmethod format-inner-node node-type [[_ inner-node] offset target-width alternative]
    ;; TODO is format-inner-node kosher here?
    (when-let [inner (format-inner-node inner-node (+ (.length ^String chars) offset) target-width alternative)]
      (l/create [chars inner]))))


(comment

  (fmt (analyze "(def ^:private n 123)"))


  -)
