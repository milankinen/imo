(ns imo.style.preserve
  (:require [imo.formatting :refer [-fmt]]
            [clojure.string :as string]))

(declare node->src)

(defn- nodes->src
  [nodes]
  (string/join "" (map node->src nodes)))

(defn- coll->source [start [_ & children :as node] end]
  (str start
       (if-let [hidden (:children (meta node))]
         (nodes->src (concat children hidden))
         (nodes->src children))
       end))

(defn- node->src [[node-type & children :as node]]
  (let [base (case node-type
               :program (nodes->src children)
               :char (str "\\" (first children))
               :number (first children)
               :boolean (str (first children))
               :string (first children)
               :keyword (first children)
               :symbol (str (first children))
               :list (coll->source "(" node ")")
               :vector (coll->source "[" node "]")
               :set (coll->source "#{" node "}")
               :map (coll->source "{" node "}")
               :ns_map (str "#" (nodes->src children) ")")
               :quote (str "'" (node->src (first children)))
               :syntax_quote (str "`" (node->src (first children)))
               :var_quote (str "#'" (node->src (first children)))
               :unquote (str "~" (node->src (first children)))
               :unquote_splice (str "~@" (node->src (first children)))
               :meta (str "^" (node->src (first children)))
               :deref (str "@" (node->src (first children)))
               :anon_fn (str "#(" (nodes->src children) ")")
               :discard (str "#_" (node->src (first children)))
               :reader_cond (str "#?" (node->src (first children)))
               :tagged_literal (str "@" (node->src (first children)))
               :regex (first children)
               :comment (str (first children) "\n")
               :space (first children)
               :newline "\n"
               :nil "nil")
        m (meta node)
        pre (map node->src (:pre m))
        post (map node->src (:post m))]
    (string/join "" (concat pre [base] post))))

(defmethod -fmt :preserve
  [_ node]
  (let [s (node->src node)]
    (if-let [col (:col (meta node))]
      (if (pos? (dec col))
        (as-> (re-pattern (str "\\n[ ]{0," (dec col) "}")) $
              (string/replace s $ "\n"))
        s)
      s)))
