(ns imo.core
  (:require [imo.reader :as reader]
            [imo.formatter :as formatter]))

(defn format-source [options input]
  {:pre  [(string? input)
          (map? options)]
   :post [(string? %)]}
  (let [ast (reader/read-ast input)
        blocks (formatter/format-ast options ast)
        output (pr-str blocks)]
    output))
