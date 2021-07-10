(ns imo.analysis.clj-exports)

(def built-in-namespaces
  '[clojure.core
    clojure.test
    clojure.string
    clojure.walk
    clojure.set])

(doseq [ns-name built-in-namespaces]
  (require ns-name))

(def exports-map
  (zipmap built-in-namespaces
          (for [ns-sym built-in-namespaces]
            (->> (ns-publics ns-sym)
                 (keys)
                 (set)))))

(def default-ns-imports
  (->> (ns-imports 'clojure.core)
       (map (juxt first #(symbol (.getName ^Class (second %)))))
       (into {})))
