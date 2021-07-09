(ns imo.forms.clojure.core.ns
  (:require [imo.analysis.core :as a]
            [imo.analysis.context :as ctx]
            [imo.logger :refer [warn]]
            [imo.config :as config]))

;; :refer-clojure

(a/defspec ::refer-clojure-filter
  (a/alt [":only" (a/vec-node (a/* ::a/simple-symbol))]
         [":exclude" (a/vec-node (a/* ::a/simple-symbol))]
         [":rename" (a/map-node (a/* [::a/simple-symbol ::a/simple-symbol]))]
         ::a/keyword))

(a/defspec ::refer-clojure [":refer-clojure" (a/* ::refer-clojure-filter)])

;; :require
; e.g. [imo.analysis.core :as a :refer [defspec]]
;      ["react" :as react]
(a/defspec ::libspec
  (a/named
    (a/vec-node
      [(a/alt ::a/simple-symbol
              ::a/string)
       (a/* (a/alt [":as" ::a/simple-symbol]
                   [":refer" (a/alt ":all" (a/vec-node (a/* ::a/simple-symbol)))]
                   [":rename" (a/map-node (a/* [::a/simple-symbol ::a/simple-symbol]))]
                   ::a/keyword))])
    "libspec"))

; e.g. imo.forms
;      "react"
(a/defspec ::direct-namespace
  (a/named
    (a/alt ::a/simple-symbol
           ::a/string)
    "namespace"))

; e.g. (clojure [core :as core]
;               [string :as string])
(a/defspec ::prefix-list
  (a/named
    (a/list-node [::a/simple-symbol (a/* (a/alt ::libspec ::direct-namespace))])
    "prefix list"))

(a/defspec ::required-lib (a/alt ::libspec ::prefix-list ::direct-namespace))
(a/defspec ::require [":require" (a/* ::required-lib)])

;; :import
; TODO
(a/defspec ::import
  [":import"])

;; ns
(a/defspec ::doc-str (a/named (a/? ::a/string) "doc string"))
(a/defspec ::attrs-map (a/named (a/? ::a/map) "attrs map"))
(a/defspec ::ns-name (a/named ::a/simple-symbol "ns name"))

(a/defspec ::ns-clause
  (a/named
    (a/list-node
      (a/alt ::refer-clojure
             ::require
             ::import))
    "ns clause"))

(a/defspec ::ns [::a/symbol ::ns-name ::doc-str ::attrs-map (a/* ::ns-clause)])

(defn- flatten-reader-conds [[node-type & children :as node]]
  (when (some? node)
    (case node-type
      :reader-cond (->> (take-nth 2 (nnext (first children)))
                        (mapcat flatten-reader-conds))
      :reader-cond-splice (->> (take-nth 2 (nnext (first children)))
                               (mapcat next)
                               (mapcat flatten-reader-conds))
      [(-> (into [node-type] (mapcat #(if (string? %) [%] (flatten-reader-conds %)) children))
           (with-meta (meta node)))])))

(defn- invalid? [node]
  (:invalid? (meta node)))

(defn- direct-ns->libspecs [[type s]]
  [{:lib s
    :js? (= :string type)}])

(defn- vec->libspecs [[_ [lib-type lib-name :as lib] & opts+flags]]
  (when-not (invalid? lib)
    (loop [libspec {:lib lib-name
                    :js? (= :string lib-type)}
           [opt & xs] opts+flags]
      (if opt
        (let [[libspec rem]
              (case (second opt)
                ":refer" (let [prev-refers (:refers libspec)
                               refers (if (or (= ":all" (second (first xs)))
                                              (= :all prev-refers))
                                        :all
                                        (->> (when-not (invalid? (first xs))
                                               (next (first xs)))
                                             (remove invalid?)
                                             (map second)
                                             (concat prev-refers)))]
                           [(assoc libspec :refers refers)
                            (next xs)])
                ":as" (let [alias (when-not (invalid? (first xs))
                                    (second (first xs)))]
                        [(assoc libspec :alias alias)
                         (next xs)])
                ":rename" (let [renames (->> (when-not (invalid? (first xs))
                                               (next (first xs)))
                                             (partition-all 2 2)
                                             (keep (fn [[oldname newname]]
                                                     (when-not (or (invalid? oldname)
                                                                   (invalid? newname))
                                                       [(second oldname) (second newname)])))
                                             (into {}))]
                            [(update libspec :renames #(merge % renames))
                             (next xs)])
                [libspec xs])]
          (recur libspec rem))
        [libspec]))))

(defn- prefix-list->libspecs [[_ prefix & xs]]
  (when-not (invalid? prefix)
    (let [prefix-s (second prefix)]
      (->> (remove invalid? xs)
           (mapcat #(case (first %)
                      (:symbol :string) (direct-ns->libspecs %)
                      :vector (vec->libspecs %)))
           (remove :js?)
           (map (fn [{:keys [lib] :as libspec}]
                  (assoc libspec :lib (str prefix-s "." lib))))))))

(defn- require->libspecs [req]
  (case (first req)
    (:symbol :string) (direct-ns->libspecs req)
    :vector (vec->libspecs req)
    :list (prefix-list->libspecs req)))

(defn- collect-clj-refers [ctx [_ _ & refer-filters]]
  (loop [clj-spec {}
         [f & xs] refer-filters]
    (if f
      (let [[clj-spec rem]
            (case (second f)
              ":only" (let [only (->> (when-not (invalid? (first xs))
                                        (next (first xs)))
                                      (remove invalid?)
                                      (map second))]
                        [(update clj-spec :only #(concat % only))
                         (next xs)])
              ":exclude" (let [exclude (->> (when-not (invalid? (first xs))
                                              (next (first xs)))
                                            (remove invalid?)
                                            (map second))]
                           [(update clj-spec :exclude #(concat % exclude))
                            (next xs)])
              ":rename" (let [renames (->> (when-not (invalid? (first xs))
                                             (next (first xs)))
                                           (partition-all 2 2)
                                           (keep (fn [[oldname newname]]
                                                   (when-not (or (invalid? oldname)
                                                                 (invalid? newname))
                                                     [(second oldname) (second newname)])))
                                           (into {}))]
                          [(update clj-spec :rename #(merge % renames))
                           (next xs)])
              [clj-spec xs])]
        (recur clj-spec rem))
      (as-> (ctx/get-ns-exports ctx 'clojure.core) exports
            (if-let [only (seq (map symbol (:only clj-spec)))]
              (filter (set only) exports)
              exports)
            (if-let [exclude (seq (map symbol (:exclude clj-spec)))]
              (remove (set exclude) exports)
              exports)
            (let [rename (or (:rename clj-spec) {})]
              (map #(let [local-s (name %)]
                      (if-let [renamed (get rename local-s)]
                        (ctx/create-binding (symbol renamed) (symbol "clojure.core" local-s))
                        (ctx/create-binding % (symbol "clojure.core" local-s))))
                   exports))))))

(defn- apply-ns [ctx ns]
  (if-not (invalid? ns)
    (let [flattened-ns (first (flatten-reader-conds ns))
          ns-name (second (nth ns 2))
          clauses (->> (nnext flattened-ns)
                       (filter #(= :list (first %)))
                       (remove invalid?))
          clj-refers (->> (first (filter #(= ":refer-clojure" (second (second %))) clauses))
                          (collect-clj-refers ctx))
          libspecs (->> (filter #(= ":require" (second (second %))) clauses)
                        (mapcat nnext)
                        (remove invalid?)
                        (mapcat require->libspecs))
          user-refers (for [libspec libspecs
                            :when (not (:js? libspec))
                            :let [lib-s (:lib libspec)
                                  lib-sym (symbol lib-s)
                                  renames (:renames libspec)
                                  refers (:refers libspec)]
                            refer (if (= :all refers)
                                    (or (ctx/get-ns-exports ctx lib-sym)
                                        (warn nil "couldn't find :all exports for " lib-sym))
                                    (map symbol refers))]
                        (let [local (if-let [renamed-s (get renames refer)]
                                      (symbol renamed-s)
                                      refer)
                              fq (symbol lib-s (name local))]
                          (ctx/create-binding local fq)))
          aliases (for [libspec libspecs
                        :when (and (not (:js? libspec))
                                   (:alias libspec))
                        :let [lib-s (:lib libspec)
                              alias (:alias libspec)]]
                    (ctx/create-alias (symbol alias) (symbol lib-s)))
          bindings (concat clj-refers user-refers)]
      (ctx/set-ns ctx ns-name aliases bindings))
    ctx))

(a/defform 'clojure.core/ns
  (fn [ctx ns]
    (let [[ctx' ns'] (a/analyze ::ns ctx ns)]
      [(apply-ns ctx' ns') ns'])))


(comment

  (repl/explain*
    (ns imo.forms.clojure.core.ns
      (:require [imo.analysis.core :as a :refer [foo bar]]
                imo.main
                [imo.main]
                "react")))

  (-> (repl/ast "(ns foo
                   #?(:cljs (:require \"react\")))")
      (flatten-reader-conds)
      (first)
      (repl/explain))

  (repl/ast "(ns foo
               (:require #?(:cljs \"react\")))")


  (repl/explain*
    (ns imo.forms.clojure.core.ns
      (:refer-clojure :exclude [dec] :rename {inc plus})
      (:require [imo.analysis.spec :as s]
                [imo.util :refer [start-of node?]]
                [imo.logger :refer [warn]]
                [imo.analysis :as a]
                (imo [core :as core]
                     [config :as config])
                imo.main
                [clojure.string :as string]))
    (string/join "," [])
    (start-of 1)
    (println config/defaults)
    (plus 1)
    (dec 1))


  '-)
