(ns imo.layout.core
  (:require [imo.util :refer [node? spaces terminal-node-types whitespace-node-types split-lines begin-chars end-chars]]
            [imo.layout.core :as l]
            [clojure.string :as string])
  (:import (java.util LinkedList)
           (java.util.regex Pattern)
           (clojure.lang ISeq)))

#_:clj-kondo/ignore
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

(defrecord Context
  [^long target-width])

(defrecord Layout
  [^Object ctx
   ^ISeq content
   ^ISeq aligns
   ^long width
   ^long max-width
   ^long lines])

(defrecord Comment [text])

(defrecord LineBreak [next-align])

(defn context? [x]
  (instance? Context x))

(defn layout? [x]
  (instance? Layout x))

(defn empty-layout [ctx]
  {:pre [(context? ctx)]}
  (->Layout ctx '() '(0) 0 0 0))

(defn column [{:keys [content width aligns]}]
  (if (string? (first content))
    (inc width)
    (inc (first aligns))))

(defn space-left
  [layout]
  (- (:target-width (:ctx layout))
     (:width layout)))

(defn width [{:keys [width max-width] :as layout}]
  {:pre [(layout? layout)]}
  (max width max-width))

(def ^:const line-break "\n")

(defn push-align [{:keys [aligns width] :as layout}]
  {:pre  [(layout? layout)]
   :post [(layout? %)]}
  (assoc layout :aligns (cons width aligns)))

(defn pop-align [layout]
  {:pre  [(layout? layout)]
   :post [(layout? %)]}
  (update layout :aligns next))

(defn add-token [layout ^String token]
  {:pre  [(layout? layout)
          (string? token)]
   :post [(layout? %)]}
  (let [content (:content layout)]
    (cond
      ;; Adding line break, reset width using latest alignment
      (= token line-break)
      (let [{:keys [ctx width max-width aligns lines]} layout
            next-align (first aligns)]
        (->Layout ctx
                  (cons (->LineBreak next-align) content)
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
      (let [{:keys [ctx width max-width aligns lines]} layout]
        (->Layout ctx
                  (cons token content)
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

(defmulti -format-node (fn [_layout node] (first node)))

(declare add-node)

(defn- format-contents-preserve-original-formatting [layout node]
  (letfn [(get-last-line-content [^LinkedList lines]
            (:content (.getLast lines)))
          (write-lines* [lines nodes]
            (doseq [node nodes]
              (write-lines lines node)))
          (write-lines [^LinkedList lines [node-type :as node]]
            (let [{:keys [pre post hidden]} (meta node)
                  _ (write-lines* lines pre)
                  content ^StringBuilder (get-last-line-content lines)]
              (when-let [chars (begin-chars node-type)]
                (.append content chars))
              (cond
                ;; newline - add new non-absolute line to the result
                (= :newline node-type)
                (.add lines {:absolute false
                             :content  (StringBuilder.)})
                ;; string - split it to lines, add first line to the current line
                ;; and remaining lines as absolute lines to the result
                (= :string node-type)
                (let [[x & xs] (split-lines (second node))]
                  (.append content x)
                  (doseq [x xs]
                    (.add lines {:absolute true
                                 :content  (doto (StringBuilder.)
                                             (.append x))})))
                ;; other terminals and whitespaces - append to the current line
                (or (contains? terminal-node-types node-type)
                    (contains? whitespace-node-types node-type))
                (.append content (second node))
                ;; other nodes - write contents recusively
                :else (write-lines* lines (remove nil? (next node))))
              (write-lines* lines hidden)
              (when-let [chars (end-chars node-type)]
                (.append ^StringBuilder (get-last-line-content lines) chars))
              (write-lines* lines post)
              lines))]
    (let [;; Meta nodes are already formatted from the top-level node, so remove
          ;; them before writing line information
          lines (->> (vary-meta node dissoc :pre :post :hidden)
                     (write-lines (LinkedList. [{:absolute false
                                                 :content  (StringBuilder.)}]))
                     (map #(update % :content str)))]
      (case (count lines)
        1 (l/add-token layout (:content (first lines)))
        (let [[x & xs] lines
              delta (- (:col (meta node)) (l/column layout))
              ;; Add first line as normal "token"
              layout (l/add-token layout (:content x))
              ;; And then add rest of the lines manually, fixing the heading white-space
              ;; from "non-absolute" lines
              raw-lines (mapv (cond
                                (pos? delta)
                                (let [re (Pattern/compile (str "^\\s{0," delta "}"))]
                                  (fn [{:keys [content absolute]}]
                                    (if-not absolute
                                      (string/replace content re "")
                                      content)))
                                (neg? delta)
                                (let [s (spaces (- delta))]
                                  (fn [{:keys [content absolute]}]
                                    (if-not absolute
                                      (str s content)
                                      content)))
                                :else :content)
                              xs)
              layout (update layout :content #(concat (interpose l/line-break (reverse raw-lines))
                                                      (cons l/line-break %)))
              ;; Finally we must update layout statistics based on the manual additions
              max-width (reduce max (l/width layout) (map count raw-lines))
              layout (assoc layout
                       :width (count (peek raw-lines))
                       :max-width max-width
                       :lines (+ (:lines layout) (count raw-lines)))]
          layout)))))

(defmethod -format-node :default [layout node]
  (format-contents-preserve-original-formatting layout node))

(defn- format-contents [layout node f]
  (let [m (meta node)]
    (if (or (:ignore? m)
            (:invalid? m))
      (format-contents-preserve-original-formatting layout node)
      (f layout node))))

(defn- add-nodes [layout nodes]
  (if nodes
    (loop [layout layout
           [node & nodes] nodes]
      (if node
        (recur (add-node layout node) nodes)
        layout))
    layout))

(defn add-node
  ([layout node] (add-node layout node -format-node))
  ([layout node f]
   {:pre [(layout? layout)
          (node? node)
          (ifn? f)]}
   (let [{:keys [pre post hidden]} (meta node)]
     (-> layout
         (add-nodes pre)
         (format-contents node f)
         (add-nodes hidden)
         (add-nodes post)))))

(defn layout->source [{:keys [content] :as layout}]
  {:pre [(layout? layout)]}
  (let [traling-comment-alignment (+ (width layout) 2)
        result (StringBuilder.)]
    (loop [[x & xs] (reverse content)
           pending-space 0
           col 0]
      (cond
        (string? x)
        (do (.append result (spaces pending-space))
            (.append result x)
            (recur xs 0 (+ col pending-space (.length ^String x))))
        (instance? LineBreak x)
        (do (.append result line-break)
            (recur xs (long (:next-align x)) 0))
        (instance? Comment x)
        (do (.append result (spaces (- traling-comment-alignment col)))
            (.append result (:text x))
            (recur xs pending-space col))
        ;; End of recursion
        (nil? x) (.toString result)))))

(defn format-preserve
  "Formats the contents of the given node trying to preserve
   the original formatting as much as possible"
  [layout node]
  (format-contents-preserve-original-formatting layout node))
