(ns imo.analysis.context
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
   ^String current-ns
   ^Keyword mode
   ^Binding recur-target])

(defn ctx? [x]
  (instance? Context x))

(defn create-binding [local-name fq-name]
  {:pre [(symbol? local-name)
         (symbol? fq-name)]}
  (->Binding local-name fq-name))

(defn create-alias [local-name target-ns]
  {:pre [(symbol? local-name)
         (symbol? target-ns)]}
  (->Alias local-name target-ns))

(defn with-alias [{:keys [aliases] :as ctx} alias]
  {:pre [(instance? Alias alias)
         (map? aliases)]}
  (assoc-in ctx [:aliases (name (:local-name alias))] alias))

(defn with-binding [{:keys [scope] :as ctx} b]
  {:pre  [(instance? Scope scope)
          (instance? Binding b)]
   :post [(ctx? %)]}
  (assoc-in ctx [:scope :bindings (name (:local-name b))] b))

(defn without-binding [{:keys [scope] :as ctx} b]
  {:pre  [(instance? Scope scope)
          (instance? Binding b)]
   :post [(ctx? %)]}
  (update-in ctx [:scope :bindings] #(dissoc % (name (:local-name b)))))

(defn push-lexical-scope [ctx]
  {:post [(ctx? %)]}
  (assoc ctx :scope (->Scope (:scope ctx) {})))

(defn pop-lexical-scope [ctx]
  {:post [(ctx? %)]}
  (update ctx :scope :parent))

(defn with-mode [ctx mode]
  {:pre  [(contains? #{:eval :quote :syntax-quote} mode)]
   :post [(ctx? %)]}
  (assoc ctx :mode mode))

(defn with-recur-target [ctx target]
  {:pre  [(or (instance? Binding target)
              (nil? target))]
   :post [(ctx? %)]}
  (assoc ctx :recur-target target))

(defn resolve-binding [{:keys [scope] :as ctx} local-name-s]
  {:pre [(ctx? ctx)
         (string? local-name-s)]}
  (loop [{:keys [bindings parent]} scope]
    (let [b (get bindings local-name-s)]
      (cond (some? b) b
            (some? parent) (recur parent)
            :else nil))))

(defn resolve-alias [{:keys [aliases] :as ctx} alias]
  {:pre [(ctx? ctx)]}
  (get aliases alias))

(defn resolve-fq-name [ctx local-name]
  {:pre [(ctx? ctx)
         (symbol? local-name)]}
  (if-let [ns (namespace local-name)]
    (if-let [alias (resolve-alias ctx ns)]
      [(symbol (name (:target-ns alias)) (name local-name)) alias]
      [local-name nil])
    (if-let [binding (resolve-binding ctx (name local-name))]
      [(:fq-name binding) binding]
      [local-name nil])))

(defn create-context [root-bindings]
  (-> {:current-ns   "user"
       :scope        (->Scope nil root-bindings)
       :aliases      {}
       :mode         :eval
       :recur-target nil}
      (map->Context)))