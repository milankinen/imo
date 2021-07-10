(ns imo.layout.core
  (:require [imo.util :refer [node? spaces]])
  (:import (clojure.lang ISeq)))

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
   :reader-cond-splice
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

(defrecord Layout
  [^ISeq content
   ^ISeq aligns
   ^long width
   ^long max-width
   ^long lines])

(defrecord Comment [text])

(defrecord LineBreak [next-align])

(defn layout? [x]
  (instance? Layout x))

(defn empty-layout []
  (->Layout '() '() 0 0 0))

(def ^:const line-break "\n")

(defn add-token [layout ^String token]
  {:pre  [(layout? layout)
          (string? token)]
   :post [(layout? %)]}
  (let [content (:content layout)]
    (cond
      ;; Adding line break, reset width using latest alignment
      (= token line-break)
      (let [{:keys [width max-width aligns lines]} layout
            next-align (or (first aligns) 0)]
        (->Layout (cons (->LineBreak next-align) content)
                  aligns
                  next-align
                  (max width max-width)
                  (inc lines)))
      ;; There was a pending comment, must add line break before we can insert the token
      (instance? Comment (first content))
      (-> (add-token layout line-break)
          (add-token token))
      ;; Otherwise we can just add the token and update width
      :else
      (let [{:keys [width max-width aligns lines]} layout]
        (->Layout (cons token content)
                  aligns
                  (+ width (.length token))
                  max-width
                  lines)))))

(defn add-line-break [layout]
  (add-token layout line-break))

(defn add-comment [layout ^String comment]
  {:pre  [(layout? layout)
          (string? comment)]
   :post [(layout? %)]}
  (let [content (:content layout)]
    (if (instance? Comment (first content))
      ;; There was a pending comment, must add line break before we can insert the next comment
      (-> (add-token layout line-break)
          (add-comment comment))
      ;; Otherwise we can just add a pending comment
      (assoc layout :content (cons (->Comment comment) content)))))

(defn push-align [{:keys [aligns width] :as layout}]
  {:pre  [(layout? layout)]
   :post [(layout? %)]}
  (assoc layout :aligns (cons width aligns)))

(defn pop-align [layout]
  {:pre  [(layout? layout)]
   :post [(layout? %)]}
  (update layout :aligns pop))

(defn width [{:keys [width max-width] :as layout}]
  {:pre [(layout? layout)]}
  (max width max-width))

(defn source-code [{:keys [contents] :as layout}]
  {:pre [(layout? layout)]}
  (let [traling-comment-alignment (+ (width layout) 2)
        result (StringBuilder.)]
    (loop [[x & xs] (reverse contents)
           pending-space 0
           col 0]
      (cond
        (string? x)
        (do (.append result (spaces pending-space))
            (.append result x)
            (recur xs 0 (+ col pending-space (.length ^String x))))
        (instance? LineBreak x)
        (do (.append result \n)
            (recur xs (long (:next-align x)) 0))
        (instance? Comment x)
        (do (.append result (spaces (- traling-comment-alignment col)))
            (.append result (:text x))
            (recur xs pending-space col))
        (nil? x) :done))
    (.toString result)))

(defmulti -add (fn [_layout node] (first node)))
