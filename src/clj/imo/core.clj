(ns imo.core
  (:refer-clojure :exclude [format])
  (:require [imo.reader :as reader]
            [imo.formatting :as formatting]
            ; require to include style formatting implementations
            [imo.style.preserve]))

(defn format [options src]
  {:pre [(string? src)
         (map? options)]}
  (let [style (:style options)
        ast (reader/read-ast src)
        ctx {:style style}
        blocks (formatting/format ctx ast)]
    (pr-str blocks)))
