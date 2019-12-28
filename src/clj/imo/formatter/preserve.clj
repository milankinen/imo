(ns imo.formatter.preserve
  (:require [clojure.string :as string]
            [imo.formatter.layout :as l]
            [imo.util :refer [split-lines]]
            [imo.ast :as ast]))

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
               :tagged_literal (str "#" (nodes->src children))
               :regex (first children)
               :comment (str (first children) "\n")
               :space (first children)
               :newline "\n"
               :nil "nil")
        m (meta node)
        pre (map node->src (:pre m))
        post (map node->src (:post m))]
    (string/join "" (concat pre [base] post))))

(defn format-node
  "Formats the given node and all its children with style
   that preserves the original formatting"
  [node output]
  {:pre [(ast/node? node)
         (l/output? output)]}
  (let [src (node->src node)
        src' (if-let [col (:col (meta node))]
               (let [re (re-pattern (str "\\n[ ]{0," (dec col) "}"))]
                 (string/replace src re "\n"))
               src)]
    (if-not (.isEmpty ^String src')
      (loop [[line & lines] (split-lines src')
             out (l/push-align output)]
        (if (not-empty lines)
          (->> (l/push-token out line)
               (l/push-newline)
               (recur lines))
          (->> (l/push-token out line)
               (l/pop-align))))
      output)))
