(ns imo.core
  (:require [imo.reader :as reader]
            [imo.formatter :as formatter]))

(defn format-source [config input]
  {:pre  [(string? input)
          (map? config)]
   :post [(string? %)]}
  (let [ast (reader/read-ast input)
        blocks (formatter/format-ast config ast)
        output (pr-str blocks)]
    output))
