(ns imo.formatter.core
  (:require [imo.util :refer [node? spaces terminal-node-types whitespace-node-types split-lines
                              begin-chars end-chars maxl may-outer-fit-one-line?]]
            [imo.layout.core :as l]
            [imo.layout.builder :as b])
  (:import (java.util LinkedList)))

(defrecord Context
  [^long target-width
   ^long alternative])

(def default-ctx
  (map->Context {:target-width 80
                 :alternative  0}))

(defn context? [x]
  (instance? Context x))

(defn format-inner-node-preserve-formatting
  "Creates a layout that tries to preserve the original node
   formatting as much as possible"
  [node]
  {:pre [(node? node)]}
  (let [alignment (dec (:col (meta node)))]
    (letfn [(get-last-line [^LinkedList lines]
              (.getLast lines))
            (write-lines* [lines nodes]
              (doseq [node nodes]
                (write-lines lines node)))
            (new-line [absolute initially-ignored-spaces]
              ^{:ignored (volatile! initially-ignored-spaces)}
              {:absolute absolute
               :content  (StringBuilder.)})
            (write-lines [^LinkedList lines [node-type :as node]]
              (let [{:keys [pre* post* children*]} (meta node)
                    _ (write-lines* lines pre*)
                    {:keys [^StringBuilder content] :as last-line} (get-last-line lines)
                    ingnored-spaces (:ignored (meta last-line))]
                (when-let [chars (begin-chars node-type)]
                  (.append content chars))
                (cond
                  ;; newline - add new non-absolute line to the result
                  (= :newline node-type)
                  (.add lines (new-line false 0))
                  ;; space - remove heading whitespace up to the column if possible
                  (= :space node-type)
                  (if (pos? (.length content))
                    (.append content ^String (second node))
                    (let [to-ignore-count (maxl 0 (- alignment @ingnored-spaces))
                          pending-count (.length ^String (second node))]
                      (when (> pending-count to-ignore-count)
                        (.append content (spaces (- pending-count to-ignore-count))))))
                  ;; string - split it to lines, add first line to the current line
                  ;; and remaining lines as absolute lines to the result
                  (= :string node-type)
                  (let [[x & xs] (split-lines (second node))]
                    (.append content x)
                    (doseq [x xs]
                      (let [line (new-line true alignment)]
                        (.append ^StringBuilder (:content line) x)
                        (.add lines line))))
                  ;; other terminals and whitespaces - append to the current line
                  (or (contains? terminal-node-types node-type)
                      (contains? whitespace-node-types node-type))
                  (.append content (second node))
                  ;; other nodes - write contents recusively
                  :else (write-lines* lines (remove nil? (next node))))
                (write-lines* lines children*)
                (when-let [chars (end-chars node-type)]
                  (.append ^StringBuilder (:content (get-last-line lines)) chars))
                (write-lines* lines post*)
                lines))]
      ;; Meta nodes are already formatted from the top-level node, so remove
      ;; them before writing line information
      (->> (vary-meta node dissoc :pre* :post* :children*)
           (write-lines (LinkedList. [(new-line false alignment)]))
           (mapv #(update % :content str))
           (l/multiline)))))

(defmulti format-inner-node (fn [node _offset _target-width _alternative] (first node)))

(defmethod format-inner-node :comment [[_ text] _ _ _]
  (l/comment text))

(defmethod format-inner-node :space [node _ _ _]
  ;; All whitespace nodes should be removed before the canonical
  ;; formatting so this implementation should not be called, unless
  ;; there is a bug somewhere.
  (throw (RuntimeException. (str "Tried to format :space node at " (pr-str (select-keys (meta node) [:col :line]))))))

(defmethod format-inner-node :newline [node _ _ _]
  ;; All whitespace nodes should be removed before the canonical
  ;; formatting so this implementation should not be called, unless
  ;; there is a bug somewhere.
  (throw (RuntimeException. (str "Tried to format :newline node at " (pr-str (select-keys (meta node) [:col :line]))))))

(defmethod format-inner-node :default [node offset target-width alternative]
  (let [layout (format-inner-node-preserve-formatting node)]
    (when (or (zero? alternative)
              (<= (l/width layout offset) target-width))
      layout)))

;;;;

(defn format-outer-node-one-line
  ([node offset target-width alternative]
   (format-outer-node-one-line node offset target-width alternative format-inner-node))
  ([node offset target-width alternative format-inner]
   (if (vector? node)
     (when (or (zero? alternative)
               (may-outer-fit-one-line? node (- target-width offset)))
       (let [{:keys [pre post]} (meta node)
             builder (b/builder offset target-width alternative)
             builder (if (seq pre)
                       (loop [b builder
                              [x & xs] pre]
                         (if (and b x)
                           (when-some [n (format-outer-node-one-line x (b/offset b) target-width alternative)]
                             (when (or (zero? (l/line-breaks n))
                                       (zero? alternative))
                               (recur (-> (b/add! b n)
                                          (b/add! 1))
                                      xs)))
                           b))
                       builder)
             builder (when builder
                       (when-some [n (format-inner node (b/offset builder) target-width alternative)]
                         (when (or (zero? (l/line-breaks n))
                                   (zero? alternative))
                           (b/add! builder n))))
             builder (if (and (seq post) builder)
                       (loop [b (b/add! builder 1)
                              [x & xs] post]
                         (if (and b x)
                           (when-some [n (format-outer-node-one-line x (b/offset b) target-width alternative)]
                             (when (or (zero? (l/line-breaks n))
                                       (zero? alternative))
                               (recur (-> (b/add! b 1)
                                          (b/add! n))
                                      xs)))
                           b))
                       builder)]
         (some-> builder (b/build!))))
     ;; spaces, breaks etc
     node)))

;; TODO trailing comments should not be aligned!
(defn format-outer-node-align-meta
  ([node offset target-width alternative]
   (format-outer-node-align-meta node offset target-width alternative format-inner-node))
  ([node offset target-width alternative format-inner]
   (if (vector? node)
     (when (or (zero? alternative)
               (may-outer-fit-one-line? node (- target-width offset)))
       (let [{:keys [pre post]} (meta node)
             builder (-> (b/builder offset target-width alternative)
                         (b/add! :align))
             builder (if (seq pre)
                       (loop [b builder
                              [x & xs] pre]
                         (if (and b x)
                           (when-some [n (format-outer-node-align-meta x (b/offset b) target-width alternative)]
                             (recur (-> (b/add! b n)
                                        (b/add! :break))
                                    xs))
                           b))
                       builder)
             builder (when builder
                       (when-some [n (format-inner node (b/offset builder) target-width alternative)]
                         (b/add! builder n)))
             builder (if (and (seq post) builder)
                       (loop [b (b/add! builder :break)
                              [x & xs] post]
                         (if (and b x)
                           (when-some [n (format-outer-node-align-meta x (b/offset b) target-width alternative)]
                             (recur (-> (b/add! b :break)
                                        (b/add! n))
                                    xs))
                           b))
                       builder)]
         (some-> builder (b/add! :dealign) (b/build!))))
     ;; spaces, breaks etc
     node)))