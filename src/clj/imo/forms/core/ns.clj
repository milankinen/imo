(ns imo.forms.core.ns
  (:require [imo.analysis.spec :as s]
            [imo.util :refer [start-of]]
            [imo.logger :refer [warn]]
            [imo.analysis :as a]
            [clojure.string :as string]))

(def ^:private ns-require-spec
  (let [as-spec (s/seq (a/keyword-node-spec ":as")
                       (a/simple-symbol-node-spec "alias"))
        mark-renames (fn [ctx node]
                       (let [renames (-> (next node)
                                         (map (comp symbol second))
                                         (partition-all 2)
                                         (map vec)
                                         (into {}))]
                         [ctx (vary-meta node :rename renames)]))
        rename-spec (s/seq (a/keyword-node-spec ":rename")
                           (a/node-spec :map "renamings"
                             (-> (s/* (s/seq (a/simple-symbol-node-spec "old-name")
                                             (a/simple-symbol-node-spec "new-name")))
                                 (s/as-analyzer mark-renames))))
        mark-as-refer (fn [ctx node]
                        [ctx (vary-meta node assoc :refer (symbol (second node)))])
        refered-sym-spec (a/simple-symbol-node-spec "refered-symbol" mark-as-refer)
        collect-vec-refers (fn [ctx node]
                             (let [refers (set (map (comp :refer meta) (next node)))]
                               [ctx (vary-meta node assoc :refer refers)]))
        refer-vec-spec (a/node-spec :vector "refers" (s/as-analyzer (s/* refered-sym-spec) collect-vec-refers))
        mark-as-refer-all (fn [ctx node] [ctx (vary-meta node assoc :refer :all)])
        refer-all-spec (a/keyword-node-spec ":all" mark-as-refer-all)
        refer-spec (s/seq (a/keyword-node-spec ":refer")
                          (s/choose
                            (a/val= ":all") refer-all-spec
                            (a/type= :vector) refer-vec-spec))
        collect-vec-libspec (fn [ctx [_ lib-ns & opts :as node]]
                              (let [lib-ns-sym (symbol (second lib-ns))
                                    libspec
                                    (loop [libspec {:namespace lib-ns-sym}
                                           [[o v] & remaining] (partition 2 opts)]
                                      (if o
                                        (recur (case (second o)
                                                 ":refer" (if-let [refer (:refer (meta v))]
                                                            (assoc libspec :refer refer)
                                                            libspec)
                                                 ":as" (assoc libspec :as (symbol (second v)))
                                                 ":rename" (if-let [rename (:rename (meta v))]
                                                             (assoc libspec :rename rename)
                                                             libspec))
                                               remaining)
                                        (update libspec
                                                :refer #(if (= % :all)
                                                          (or (a/get-ns-exports ctx lib-ns-sym)
                                                              (warn nil "couldn't find :all exports for " lib-ns-sym))
                                                          %))))]
                                [ctx (vary-meta node assoc :libspec libspec)]))
        vec-libspec (a/node-spec :vector "libspec"
                      (-> (s/seq (a/simple-symbol-node-spec "lib")
                                 (s/* (s/choose
                                        (a/val= ":refer") refer-spec
                                        (a/val= ":rename") rename-spec
                                        (a/val= ":as") as-spec)))
                          (s/as-analyzer collect-vec-libspec)))
        collect-sym-libspec (fn [ctx node]
                              (let [libspec {:namespace (symbol (second node))}]
                                [ctx (vary-meta node assoc :libspec libspec)]))
        sym-libspec (a/simple-symbol-node-spec "libname" collect-sym-libspec)
        collect-prefix-list (fn [ctx [_ prefix-node & libspec-nodes :as node]]
                              (let [prefix (str (second prefix-node) ".")
                                    libspecs (->> (map (comp :libspec meta) libspec-nodes)
                                                  (map (fn [libspec]
                                                         (update libspec :namespace #(symbol (str prefix %))))))]
                                [ctx (vary-meta node assoc :libspecs libspecs)]))
        prefix-list (a/node-spec :list "prefix-list"
                      (-> (s/seq (a/simple-symbol-node-spec "prefix")
                                 (s/+ (s/choose
                                        (a/type= :vector) vec-libspec
                                        (a/type= :symbol) sym-libspec)))
                          (s/as-analyzer collect-prefix-list)))
        flag (a/keyword-node-spec "flag" (fn [ctx node] [ctx (vary-meta node assoc :flag (symbol (second node)))]))]
    (s/seq (a/keyword-node-spec ":require")
           (s/+ (s/choose
                  (a/type= :vector) vec-libspec
                  (a/type= :symbol) sym-libspec
                  (a/type= :list) prefix-list
                  (a/type= :keyword) flag)))))

(def ^:private ns-refer-clojure-spec
  (let [syms-as (fn [key]
                  (fn [ctx [_ & children :as node]]
                    (let [syms (map (comp symbol second) children)]
                      [ctx (vary-meta node assoc key syms)])))
        only-spec (s/seq (a/keyword-node-spec ":only")
                         (a/node-spec :vector "requires"
                           (-> (s/* (a/simple-symbol-node-spec "require"))
                               (s/as-analyzer (syms-as :only)))))
        exclude-spec (s/seq (a/keyword-node-spec ":exclude")
                            (a/node-spec :vector "requires"
                              (-> (s/* (a/simple-symbol-node-spec "require"))
                                  (s/as-analyzer (syms-as :exclude)))))
        rename-spec (s/seq (a/keyword-node-spec ":rename")
                           (a/node-spec :map "renames"
                             (-> (s/* (s/seq (a/simple-symbol-node-spec "old-name")
                                             (a/simple-symbol-node-spec "new-name")))
                                 (s/as-analyzer (fn [ctx [_ & children :as node]]
                                                  (let [renames (->> (map (comp symbol second) children)
                                                                     (partition 2)
                                                                     (map vec)
                                                                     (into {}))]
                                                    [ctx (vary-meta node assoc :rename renames)]))))))]
    (s/seq (a/keyword-node-spec ":refer-clojure")
           (s/* (s/choose
                  (a/val= ":only") only-spec
                  (a/val= ":exclude") exclude-spec
                  (a/val= ":rename") rename-spec)))))


(def ^:private ns-import-spec
  (let [single-class-spec (a/simple-symbol-node-spec
                            "class-name"
                            (fn [ctx node]
                              (let [parts (string/split (second node) #"\.")
                                    [pkg cls] (if (= 1 (count parts))
                                                [nil (first parts)]
                                                [(string/join "." (pop parts))
                                                 (peek parts)])
                                    import {:package pkg
                                            :class   cls}]
                                [ctx (vary-meta node assoc :imports [import])])))
        class-list-spec (a/node-spec :list "package-list"
                          (-> (s/seq (a/simple-symbol-node-spec "package-name")
                                     (s/+ (a/simple-symbol-node-spec "")))
                              (s/as-analyzer
                                (fn [ctx [_ pkg-node & class-nodes :as node]]
                                  (let [pkg (second pkg-node)
                                        classes (map second class-nodes)
                                        imports (map (fn [c] {:package pkg :class c}) classes)]
                                    [ctx (vary-meta node assoc :imports imports)])))))]
    (s/seq (a/keyword-node-spec ":import")
           (s/* (s/choose
                  (a/type= :symbol) single-class-spec
                  (a/type= :list) class-list-spec)))))

(def ^:private ns-use-spec
  (s/seq (a/keyword-node-spec ":use")
         (s/custom (fn [parent input]
                     (warn (start-of parent) ":use is not supported (at least for now), use :require instead")
                     input))
         (s/* (a/any-node-spec "libspec"))))

(def ^:private ns-gen-class-spec
  (s/seq (a/keyword-node-spec ":gen-class")
         (s/* (a/any-node-spec "option"))))

(def ^:private ns-load-spec
  (s/seq (a/keyword-node-spec ":load")
         (s/* (a/string-node-spec "lib"))))

(def ^:private ns-refer-spec
  (s/seq (a/keyword-node-spec ":refer")
         (a/simple-symbol-node-spec "lib")
         (s/custom (fn [parent input]
                     (warn (start-of parent) ":refer is not supported (at least for now)")
                     input))
         (s/* (a/any-node-spec "filter"))))

(def ^:private ns-clause-analyzer
  (let [collect-meta (fn [ctx [_ clause & args :as node]]
                       [ctx (case (second clause)
                              nil node
                              ":require" (let [requires (mapcat
                                                          #(let [m (meta %)]
                                                             (cond
                                                               (:libspecs m) (:libspecs m)
                                                               (:libspec m) [(:libspec m)]
                                                               :else nil))
                                                          args)]
                                           (vary-meta node assoc :clause :require :requires requires))
                              ":refer-clojure" (let [spec (->> (map #(select-keys (meta %) [:only :exclude :rename]) args)
                                                               (reduce merge {}))]
                                                 (vary-meta node assoc :clause :refer-clojure :spec spec))
                              ":import" (let [imports (mapcat (comp :imports meta) args)]
                                          (vary-meta node assoc :clause :import :imports imports))
                              (let [clause (keyword (subs (second clause) 1))]
                                (vary-meta node assoc :clause clause)))])]
    (-> (s/choose
          (a/val= ":require") ns-require-spec
          (a/val= ":refer-clojure") ns-refer-clojure-spec
          (a/val= ":import") ns-import-spec
          (a/val= ":gen-class") ns-gen-class-spec
          (a/val= ":use") ns-use-spec
          (a/val= ":refer") ns-refer-spec
          (a/val= ":load") ns-load-spec)
        (s/as-analyzer collect-meta))))

(def ^:private ns-analyzer
  (letfn [(analyze-ns [ctx [_ _ ns-name-node _ _ & clause-nodes :as node]]
            (let [ns-name (second ns-name-node)
                  requires (mapcat (comp :requires meta) clause-nodes)
                  aliases (keep (fn [{:keys [as namespace]}]
                                  (some-> as (a/create-alias namespace)))
                                requires)
                  clj-spec (first (keep #(let [m (meta %)]
                                           (when (= :refer-clojure (:clause m))
                                             (:spec m)))
                                        clause-nodes))
                  clj-exports (as-> (a/get-ns-exports ctx 'clojure.core) exports
                                    (if-let [only (seq (:only clj-spec))]
                                      (filter (set only) exports)
                                      exports)
                                    (if-let [exclude (seq (:exclude clj-spec))]
                                      (remove (set exclude) exports)
                                      exports)
                                    (let [rename (or (:rename clj-spec) {})]
                                      (map #(if-let [renamed (get rename %)]
                                              (a/create-binding renamed (symbol "clojure.core" (name %)))
                                              (a/create-binding % (symbol "clojure.core" (name %))))
                                           exports)))
                  imports (->> (mapcat (comp :imports meta) clause-nodes)
                               (mapcat (fn [{:keys [package class]}]
                                         (let [fq-s (if package
                                                      (str package "." class)
                                                      class)]
                                           [; class name
                                            (a/create-binding (symbol class) (symbol fq-s))
                                            ; ctor
                                            (a/create-binding (symbol (str class ".")) (symbol (str fq-s ".")))]))))
                  bindings (-> (mapcat (fn [{:keys [refer rename namespace]}]
                                         (map #(let [local-name (get rename % %)
                                                     fq-name (symbol (str namespace) (str %))]
                                                 (a/create-binding local-name fq-name))
                                              refer))
                                       requires)
                               (concat clj-exports imports))
                  ctx-in-ns (a/set-ns ctx ns-name aliases bindings)]
              [ctx-in-ns node]))]
    (-> (s/seq (a/symbol-node-spec "invocation")
               (a/simple-symbol-node-spec "ns-name")
               (s/? (a/string-node-spec "doc-string"))
               (s/? (a/map-node-spec "attr-map"))
               (s/* (a/node-spec :list "ns-clause" ns-clause-analyzer)))
        (s/as-analyzer analyze-ns))))

(a/set-form-analyzer! 'clojure.core/ns ns-analyzer)

