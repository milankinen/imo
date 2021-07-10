(ns imo.analysis.context
  (:require [imo.analysis.clj-exports :as clj-exports])
  (:import (clojure.lang Symbol Keyword)
           (java.util Map)))

(defrecord Binding
  [^Symbol local-name
   ^Symbol fq-name])

(defrecord Alias
  [^Symbol local-name
   ^Symbol target-ns])

(defrecord Scope [parent ^Map bindings])

(defrecord Context
  [^Scope scope
   ^Map aliases
   ^Map sym-resolution
   ^Map ns-exports
   ^String current-ns
   ^Keyword mode
   ^Binding recur-target])

(defn ctx?
  "Returns boolean whether the given value is context or not"
  [x]
  (instance? Context x))

(defn create-binding
  "Creates new binding with the given fully qualified and local name"
  [local-name fq-name]
  {:pre [(simple-symbol? local-name)
         (symbol? fq-name)]}
  (->Binding local-name fq-name))

(defn create-alias
  "Creates alias pointing to the given namespace"
  [local-name target-ns]
  {:pre [(simple-symbol? local-name)
         (simple-symbol? target-ns)]}
  (->Alias local-name target-ns))

(defn add-alias
  "Adds alias to the context and returns the updated context"
  [{:keys [aliases] :as ctx} alias]
  {:pre [(instance? Alias alias)
         (map? aliases)]}
  (assoc-in ctx [:aliases (name (:local-name alias))] alias))

(defn add-binding
  "Adds binding to the context's current lexical scope and returns
   the updated context"
  [{:keys [scope] :as ctx} b]
  {:pre  [(instance? Scope scope)
          (instance? Binding b)]
   :post [(ctx? %)]}
  (assoc-in ctx [:scope :bindings (name (:local-name b))] b))

(defn push-lexical-scope
  "Pushes new lexical scope the the context"
  [ctx]
  {:pre  [(ctx? ctx)]
   :post [(ctx? %)]}
  (assoc ctx :scope (->Scope (:scope ctx) {})))

(defn pop-lexical-scope
  "Pops the latest lexical scope and all bindings added on it"
  [ctx]
  {:pre  [(ctx? ctx)]
   :post [(ctx? %)]}
  (update ctx :scope :parent))

(defn set-mode
  "Sets new mode to the context and returns the updated context"
  [ctx mode]
  {:pre  [(contains? #{:eval :quote :syntax-quote} mode)
          (ctx? ctx)]
   :post [(ctx? %)]}
  (assoc ctx :mode mode))

(defn set-recur-target
  "Sets new recur target to the context and returns the
   updated context"
  [ctx target]
  {:pre  [(or (instance? Binding target)
              (nil? target))]
   :post [(ctx? %)]}
  (assoc ctx :recur-target target))

(defn resolve-binding
  "Resolves binding based on it's local name"
  [{:keys [scope] :as ctx} local-name]
  {:pre [(ctx? ctx)
         (simple-symbol? local-name)]}
  (loop [{:keys [bindings parent]} scope]
    (let [b (get bindings (name local-name))]
      (cond (some? b) b
            (some? parent) (recur parent)
            :else nil))))

(defn resolve-alias
  "Resolves alias for the given name"
  [{:keys [aliases] :as ctx} alias]
  {:pre [(ctx? ctx)
         (string? alias)]}
  (get aliases alias))

(defn resolve-fq-name
  "Resolves fully qualified name for the given local name
   and returns a tuple of `[fq-name source]` where source
   can be either alias or binding or `nil` if fully qualified
   name can't be resolved"
  [ctx local-name]
  {:pre [(ctx? ctx)
         (symbol? local-name)]}
  (if-let [ns (namespace local-name)]
    (if-let [alias (resolve-alias ctx ns)]
      [(symbol (name (:target-ns alias)) (name local-name)) alias]
      [local-name nil])
    (if-let [binding (resolve-binding ctx local-name)]
      [(:fq-name binding) binding]
      [local-name nil])))

(defn- indexed-bindings [bindings]
  (into {} (map (fn [b] [(name (:local-name b)) b]) bindings)))

(def default-clj-core-bindings
  (->> (get clj-exports/exports-map 'clojure.core)
       (map #(create-binding % (symbol "clojure.core" (name %))))))

(def default-import-bindings
  (for [[import-sym fq-class-name-sym] clj-exports/default-ns-imports
        suffix ["" "."]
        :let [local-name (symbol (str import-sym suffix))
              fq-name (symbol (str fq-class-name-sym suffix))]]
    (create-binding local-name fq-name)))

(def default-import-aliases
  (for [[import-sym fq-class-name-sym] clj-exports/default-ns-imports]
    (->Alias import-sym fq-class-name-sym)))

(def default-bindings
  (-> (concat default-clj-core-bindings default-import-bindings)
      (indexed-bindings)))

(def default-aliases
  (zipmap (map (comp str :local-name) default-import-aliases)
          default-import-aliases))

(defn create-context
  "Creates fresh context with the given custom symbol resolutions
   and namespace exports"
  [symbol-resolution ns-exports]
  {:pre [(map? symbol-resolution)
         (map? ns-exports)]}
  (-> {:current-ns     "user"
       :scope          (->Scope nil default-bindings)
       :aliases        default-aliases
       :sym-resolution symbol-resolution
       :mode           :eval
       :recur-target   nil}
      (map->Context)))

(defn set-ns
  "Resets the namespace for the given context"
  [ctx ns-name aliases bindings]
  {:pre [(ctx? ctx)
         (string? ns-name)
         (coll? aliases)
         (every? #(instance? Alias %) aliases)
         (coll? bindings)
         (every? #(instance? Binding %) bindings)]}
  (assoc ctx :current-ns ns-name
             :scope (->Scope nil (indexed-bindings bindings))
             :aliases (into {} (map (fn [a] [(name (:local-name a)) a]) aliases))))

(defn get-ns-exports
  "Returns a set of fully qualified exports for the given
   namespace or `nil` if exports are not known"
  [ctx ns-name]
  {:pre [(simple-symbol? ns-name)]}
  (or (get clj-exports/exports-map ns-name)
      (get (:ns-exports ctx) ns-name)))
