(ns imo.formatting
  (:require [imo.logger :refer [timed]]
            [imo.util :refer [node? node->source spaces]]
            [imo.formatting.preserve-formatting :refer [layout-preserve]]
            [imo.formatting.core :as c]))

(def ^:private node-types
  [:anon-fn
   :boolean
   :char
   :deref
   :discard
   :keyword
   :list
   :map
   :meta
   :nil
   :ns-map
   :number
   :quote
   :reader-cond
   :regex
   :set
   :string
   :symbol
   :symbolic-val
   :syntax-quote
   :tagged-literal
   :unquote
   :unquote-splice
   :var-quote
   :vector])

(doseq [type node-types]
  (defmethod c/layout type [node]
    (layout-preserve node)))

(def ^:private spaces-before-comment
  (spaces 2))

(defn- format-top-level-node! [^StringBuilder sb width node]
  (let [result (c/fit (c/layout node) width)
        code (c/render result)
        resolved-as (:resolve-as (meta node))]
    (.append sb ^String code)
    (or (and (or (= 'def resolved-as)
                 (= 'clojure.core/declare resolved-as))
             (not (c/multiline? result)))
        (= :meta (first node)))))

(defn- append-newlines! [^StringBuilder sb n]
  (dotimes [_ n]
    (.append sb "\n")))

(defn- format-root! [^StringBuilder sb width [_ & forms :as root-node]]
  (let [m (meta root-node)
        nodes (concat (:pre m)
                      (mapcat #(let [m (meta %)
                                     pre (:pre m)
                                     hidden (:hidden m)
                                     post (:post m)]
                                 (if (or pre hidden post)
                                   (let [node (vary-meta % dissoc :pre :hidden :post)]
                                     (concat pre [node] hidden post))
                                   [%]))
                              forms)
                      (:hidden m)
                      (:post m))]
    (loop [[node & rem :as nodes] (seq nodes)
           needs-empty-line? false
           prev-comment? true
           newlines 0]
      (if nodes
        (case (first node)
          :space (recur rem needs-empty-line? prev-comment? newlines)
          :newline (recur rem needs-empty-line? prev-comment? (min 3 (inc newlines)))
          :comment (do (when (and (pos-int? (.length sb))
                                  (not prev-comment?))
                         (if (zero? newlines)
                           (.append sb spaces-before-comment)
                           (append-newlines! sb (max 2 newlines))))
                       (.append sb (second node))
                       (recur rem false true 1))
          (do (when (pos-int? (.length sb))
                (as-> (max newlines (if needs-empty-line? 2 1)) required
                      (if prev-comment? (dec required) required)
                      (append-newlines! sb required)))
              (let [compact? (format-top-level-node! sb width node)]
                (recur rem (not compact?) false 0))))
        ; Ensure that file ends with empty line
        (when (and (pos-int? (.length sb))
                   (not prev-comment?))
          (.append sb "\n"))))))

;;
;;

(defn format-ast
  "Formats the given ast using the supplied config and returns
   the formatted source as a string"
  [config ast]
  {:pre [(node? ast)
         (= :$ (first ast))]}
  (let [width (:width config)
        sb (StringBuilder.)]
    (timed "formatting"
      (format-root! sb width ast)
      (.toString sb))))
