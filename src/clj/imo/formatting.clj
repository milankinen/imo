(ns imo.formatting
  (:require [imo.util :refer [node? node->source]]
            [clojure.string :as string]))

(def ^:private non-groupable-top-level-forms
  '#{clojure.core/defn
     clojure.core/defn-
     clojure.core/ns
     clojure.core/comment
     clojure.core/defmulti
     clojure.core/defmacro
     clojure.core/defprotocol
     clojure.core/defrecord
     clojure.core/deftype
     clojure.core/definterface
     clojure.core/defstruct})

(defn- groupable? [form]
  (not (contains? non-groupable-top-level-forms (:resolve-as (meta form)))))

(defn- make-newlines [num-newlines]
  (string/join "" (repeat num-newlines "\n")))

(defn- format-top-level-form [form]
  (node->source form))

(defn format-root
  "Formats the given root ast node trying to fit the output
   to the given target width as well as possible"
  [width [node-type & forms :as root-node]]
  {:pre [(pos-int? width)
         (node? root-node)
         (= :$ node-type)]}
  (let [{:keys [pre post hidden]} (meta root-node)
        nodes (volatile! (concat pre forms hidden post))
        last-appended-node (volatile! nil)
        newlines (volatile! 0)
        result (StringBuilder.)]
    (while (seq @nodes)
      (let [node (first @nodes)]
        (case (first node)
          ;; Ignore spaces
          :space (vswap! nodes next)
          ;; Collect all subsequent newlines, but limit their amount
          :newline
          (while (= :newline (ffirst @nodes))
            (vswap! newlines #(min 3 (inc %)))
            (vswap! nodes next))
          :comment
          (let [comment (string/replace (second node) #"\n$" "")]
            (case (first @last-appended-node)
              ;; Nothing formatted yet, just append this comment
              nil (do (.append result comment)
                      (vreset! last-appended-node node))
              ;; Last formatted node was comment: add this comment to the
              ;; one of the next lines, depending on how many newlines
              ;; the user has added between comments
              :comment
              (do (.append result (make-newlines (max 1 @newlines)))
                  (.append result comment)
                  (vreset! last-appended-node node))
              ;; Last formatted node was form: if user has added newlines between
              ;; form and comment, then respect that decision. If not, add two
              ;; spaces between form and comment
              :form
              (do (.append result (if (pos? @newlines)
                                    (make-newlines (max 2 @newlines))
                                    "  "))
                  (.append result comment)
                  (when (pos? @newlines)
                    (vreset! last-appended-node node))))
            (vreset! newlines 1)
            (vswap! nodes next))
          :form
          (let [[_ form] node
                formatted (format-top-level-form form)]
            (case (first @last-appended-node)
              ;; Nothing formatted yet, just append this form to the
              ;; beginning of the file
              nil (.append result formatted)
              ;; Last formatted node was comment, it means that it has at lest
              ;; one newline that must be rendered before form, so lets render
              ;; newlines and then the top level form
              :comment
              (do (.append result (make-newlines @newlines))
                  (.append result formatted))
              ;; Last formatted node was top level form: if this form is groupable
              ;; with the last formatted form, we can use only one newline between
              ;; forms, othewise we must add single blank line between them
              :form
              (let [group? (and (groupable? (second @last-appended-node))
                                (groupable? form)
                                (not (string/includes? formatted "\n"))
                                (not (string/includes? (nth @last-appended-node 2) "\n")))
                    min-newlines (if group? 1 2)]
                (.append result (make-newlines (max min-newlines @newlines)))
                (.append result formatted)))
            (vreset! newlines 0)
            (vreset! last-appended-node (conj node formatted))
            (vswap! nodes next))
          ;; Every other node: mark actual node as "top level form"
          ;; and flatten its meta nodes to top level
          (let [form (first @nodes)
                {:keys [pre post hidden]} (meta form)
                bare-form (vary-meta form dissoc :pre :post :hidden)]
            (vswap! nodes #(concat pre [[:form bare-form]] hidden post (next %)))))))
    ;; End non-empty sources with newline
    (when (pos? (.length result))
      (.append result "\n"))
    (.toString result)))
