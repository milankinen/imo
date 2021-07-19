(ns imo.formatter.terminals-formatter
  (:require [imo.layout :as l]
            [imo.util :refer [split-lines terminal-node-types]]
            [imo.formatter.core :refer [format-node]]))

(defn format-terminal [[_ ^String s] offset target-width alternative]
  (when (or (zero? alternative)
            (<= (+ offset (.length s)) target-width))
    s))

(defn format-string [[_ ^String s] offset target-width alternative]
  (let [lines (split-lines s)
        layout (case (count lines)
                 1 (l/token s)
                 (->> (for [line (next lines)]
                        {:absolute true
                         :content  line})
                      (cons {:absolute false
                             :content  (first lines)})
                      (l/multiline)))]
    (when (or (zero? alternative)
              (<= (l/width layout offset) target-width))
      layout)))

(doseq [node-type terminal-node-types
        :when (not= :string node-type)]
  (defmethod format-node node-type [node offset target-width alternative]
    (format-terminal node offset target-width alternative)))

(defmethod format-node :string [node offset target-width alternative]
  (format-string node offset target-width alternative))


(comment


  (format* {:foo
            12})

  (format* 123)

  (format (analyze "[112312123 2 ; comment \nasd]"))
  (format* [124 123 123 124 123 123 124 123 123 124 123 123 124 123 123 124 123 123 124 123 123 124 123 123])

  -)
