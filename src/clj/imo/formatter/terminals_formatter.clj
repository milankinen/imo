(ns imo.formatter.terminals-formatter
  (:require [imo.layout.core :as l]
            [imo.util :refer [split-lines terminal-node-types]]
            [imo.formatter.core :refer [format-inner-node]]))

(doseq [node-type terminal-node-types
        :when (not= :string node-type)]
  (defmethod format-inner-node node-type [[_ ^String s] offset target-width alternative]
    (when (or (zero? alternative)
              (<= (+ offset (.length s)) target-width))
      s)))

(defmethod format-inner-node :string [[_ ^String s] offset target-width alternative]
  (let [lines (split-lines s)
        layout (case (count lines)
                 1 s
                 (->> (for [line (next lines)]
                        {:absolute true
                         :content  line})
                      (cons {:absolute false
                             :content  (first lines)})
                      (l/multiline)))]
    (when (or (zero? alternative)
              (<= (l/width layout offset) target-width))
      layout)))


(comment

  (fmt* 1)
  (fmt* :foo/bar)
  (fmt* "tsers")
  (fmt* 'simple-symbol?)

  -)
