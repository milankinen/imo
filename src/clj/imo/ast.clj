(ns imo.ast
  (:require [clojure.string :as string])
  (:import (imo SourceReader)))

(defn- normalize-newlines [s]
  (string/replace s #"(\r\n|\r)" "\n"))

(defn source->ast
  "Source string to AST"
  [source]
  {:pre [(string? source)]}
  (-> (normalize-newlines source)
      (SourceReader/readAst)))

(defmulti
  ast->source
  "AST to source string"
  (fn [ast]
    (cond
      (vector? ast) (first ast)
      (string? ast) :*str*
      :else :*none*)))

(defn whitespace?
  "Tells whether the given node is whitespace or not"
  [[type :as node]]
  (and (vector? node)
       (or (= :space type)
           (= :newline type))))

(defn form?
  "Tells whether the given node is non-whitespace or not"
  [node]
  (and (vector? node)
       (not (whitespace? node))))

(defn forms
  "Returns non-whitespace nodes from the give node list"
  [nodes]
  {:pre [(sequential? nodes)]}
  (remove whitespace? nodes))

(def ^{:doc "Shortcut for newline AST node"} nl
  [:newline])

(defn- s-join
  [nodes]
  (->> (map ast->source nodes)
       (apply str)))

(defmethod ast->source :*str* [ast] ast)

(defmethod ast->source :*none* [_] "")

(defmethod ast->source :program [[_ & children]]
  (s-join children))

(defmethod ast->source :char [[_ char]]
  (str \\ char))

(defmethod ast->source :list [[_ & children]]
  (str "(" (s-join children) ")"))

(defmethod ast->source :vector [[_ & children]]
  (str "[" (s-join children) "]"))

(defmethod ast->source :map [[_ & children]]
  (str "{" (s-join children) "}"))

(defmethod ast->source :set [[_ & children]]
  (str "#{" (s-join children) "}"))

(defmethod ast->source :ns_map [[_ prefix & children]]
  (str "#" prefix (s-join children)))

(defmethod ast->source :quote [[_ & children]]
  (str "'" (s-join children)))

(defmethod ast->source :syntax_quote [[_ & children]]
  (str "`" (s-join children)))

(defmethod ast->source :meta [[_ & children]]
  (str "^" (s-join children)))

(defmethod ast->source :deref [[_ & children]]
  (str "@" (s-join children)))

(defmethod ast->source :unquote_splice [[_ & children]]
  (str "~@" (s-join children)))

(defmethod ast->source :unquote [[_ & children]]
  (str "~" (s-join children)))

(defmethod ast->source :var_quote [[_ & children]]
  (str "#'" (s-join children)))

(defmethod ast->source :anon_fn [[_ & children]]
  (str "#(" (s-join children) ")"))

(defmethod ast->source :ignore [[_ & children]]
  (str "#_" (s-join children)))

(defmethod ast->source :symbolic_val [[_ & children]]
  (str "##" (s-join children)))

(defmethod ast->source :reader_cond [[_ & children]]
  (str "#?" (s-join children)))

(defmethod ast->source :tagged_literal [[_ & children]]
  (str "#" (s-join children)))

(defmethod ast->source :regexp [[_ value]]
  value)

(defmethod ast->source :comment [[_ & comment]]
  (str "; " comment))

(defmethod ast->source :nil [_]
  "nil")

(defmethod ast->source :space [[_ ws]]
  (if (integer? ws)
    (apply str (repeat ws " "))
    ws))

(defmethod ast->source :newline [_] "\n")

(defmethod ast->source :default [ast]
  (s-join ast))




(comment

  (read-string "lol-bal/ba/ba.")

  (def src "

  `(asd ~@(form asd) ~foo)

 @#_    asd
  @as
  ")

  (-> (source->ast src)
      #_(conj [:space 10])
      #_(ast->source))


  '-)
