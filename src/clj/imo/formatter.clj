(ns imo.formatter
  (:require [imo.formatter.preserve :as preserve]
            [imo.logger :refer [vvv]]
            [imo.formatter.layout
             :refer [-node-layout Layout best inline align | ! $]
             :as l]
            [imo.ast :as ast]
            [imo.util :refer [match]])
  (:import (clojure.lang Symbol)))

(def ^:private ^:dynamic *config* nil)

;;
;; Base
;;

(defn- fmt-preserve [node out]
  (preserve/format-node node out))

(defn- fmt-default [node out]
  (fmt-preserve node out))

(defn- log-fmt-time [node start-t end-t]
  (let [out (l/empty-output)
        node-s (l/to-src (fmt-preserve node out))
        t (/ (- end-t start-t) 1000000.0)]
    (format "Took %.2f ms to format form:\n%s" t node-s)))

(defn- fmt [node out]
  (let [start-t (System/nanoTime)
        lo (-node-layout node)
        res (l/fit lo *config* out)
        end-t (System/nanoTime)]
    (vvv (log-fmt-time node start-t end-t))
    res))


;;
;; Primitive layouts
;;

(defmethod -node-layout :number [[_ value-s]]
  ($ value-s))

(defmethod -node-layout :boolean [[_ ^Boolean value]]
  ($ (str value)))

(defmethod -node-layout :string [[_ value-s]]
  ($ value-s))

(defmethod -node-layout :symbol [[_ ^Symbol value]]
  ($ (str value)))

(defmethod -node-layout :keyword [[_ value-s]]
  ($ value-s))

;;
;; List layouts
;;

(defn- default-list-layout [[_ & items]]
  ($ (inline (interpose 1 items))
     (align items)))

(defn- list-layout-dispatch [node]
  (let [first-child (second node)]
    (when (ast/symbol? first-child)
      (:resolved (meta first-child)))))

(defmulti -list-layout list-layout-dispatch)

(defmethod -list-layout :default [_] nil)

(defn- list-layout [node]
  (let [lo (if (> (count node) 1)
             (or (-list-layout node)
                 (default-list-layout node))
             (default-list-layout node))]
    ($ ["(" lo ")"])))

(defmethod -node-layout :list [node]
  (list-layout node))

;;
;; Vector layouts
;;

(defmethod -node-layout :vector [[_ & items]]
  (case (count items)
    0 ($ ["[" "]"])
    1 ($ ["[" (first items) "]"])
    ($ ["[" (inline (interpose 1 items)) "]"]
       ["[" (align items) "]"])))

;;
;; Special list form formatting by symbol
;;

(defmethod -list-layout 'clojure.core/ns [[_ & nodes]]
  (letfn [(layout-ref [ref]
            (or (when (ast/list? ref)
                  (when-let [[kw args] (match (next ref) '[keyword _+])]
                    (case (second kw)
                      ":require" ($ ["(" kw 1 (align args) ")"])
                      nil)))
                ; default formatting
                ref))]
    (when-let [[ns ns-name doc-str attr-map refs] (match nodes '[_ symbol string? map? _*])]
      (| [ns 1 ns-name]
         [1 doc-str]
         [1 attr-map]
         [1 (align (map (comp best layout-ref) refs))]))))

;;
;;

(defn- fmt-top-level-form [form out]
  (cond
    (ast/newline? form)
    (l/push-newline out)

    (ast/space? form)
    (l/push-token out (second form))

    (ast/comment? form)
    (-> (l/push-token out (second form))
        (l/push-newline))

    (and (ast/list? form)
         (= 'clojure.core/ns (list-layout-dispatch form)))
    (fmt form out)

    :else
    (fmt-preserve form out)))

;;
;; Public stuff
;;

(defn format-ast
  "Formats the given node and all its children"
  [config [_ & children :as program]]
  (binding [*config* config]
    (vvv "------------------------")
    (vvv "Start formatting...")
    (let [start-t (System/nanoTime)
          forms (concat children (:post (meta program)))
          out (l/empty-output)
          src (->> (mapcat ast/flatten forms)
                   (reduce #(fmt-top-level-form %2 %1) out)
                   (l/to-src))
          end-t (System/nanoTime)]
      (vvv (format "Format complete, took: %.2f ms" (/ (- end-t start-t) 1000000.0)))
      (vvv "------------------------")
      src)))

(comment

  (def a
    (repl/ast "|(ns foo.bar \"This is doc!\"
               |  (:require [clojure.string :as string] [clojure.walk :refer [postwalk]])
               |(:import (java.util Date))
               | (:refer-clojure :exclude [map]))
               |(defn lol []) ; Tsers
               |; Tsers
               |
               |
               |(defn bal []
               |  123)
               |#_..."))

  (def a (repl/ast "|(ns foo)
                    |(defn foobar []
                    |  123)"))

  (require 'imo.config)
  (-> (format-ast (assoc imo.config/defaults :width 80) a)
      (println))


  (def s (ctx/create-ctx imo.config/defaults))

  (fmt-default s (repl/ast "\n"))

  (repl/pr-ast (repl/ast "(ns)\n\n"))


  (repl/pr-ast a)


  '-)