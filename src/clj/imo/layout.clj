(ns imo.layout
  (:refer-clojure :exclude [comment empty])
  (:require [imo.util :refer [maxl spaces]]
            [clojure.string :as string])
  (:import (clojure.lang IPersistentVector)
           (imo LayoutBuilder)))

(defprotocol ILayout
  :extend-via-metadata true
  (-flatten [_])
  (-relative-offset [_])
  (-relative-width [_])
  (next-offset [_ offset])
  (width [_ offset])
  (line-breaks [_]))

(extend-protocol ILayout
  String
  (-flatten [^String s] [s])
  (-relative-offset [^String s]
    (.length s))
  (-relative-width [^String s]
    (.length s))
  (next-offset [^String s offset]
    (+ (.length s) offset))
  (width [^String s offset]
    (+ (.length s) offset))
  (line-breaks [_]
    0))

(extend-protocol ILayout
  IPersistentVector
  (-flatten [v] v)
  (-relative-offset [v]
    (loop [aligns '(0)
           offset 0
           [x & xs :as v] (seq v)]
      (if v
        (cond
          (integer? x) (recur aligns (long (+ x offset)) xs)
          (= :break x) (recur aligns (long (first aligns)) xs)
          (= :align x) (recur (cons offset aligns) (long offset) xs)
          (= :align-indent x) (recur (cons (inc offset) aligns) (long offset) xs)
          (= :dealign x) (recur (next aligns) offset xs)
          :else
          (let [rel-o (-relative-offset x)]
            (if (>= rel-o 0)
              (recur aligns (long (+ offset rel-o)) xs)
              -1)))
        offset)))
  (-relative-width [v]
    (loop [max-w 0
           aligns '(0)
           offset 0
           [x & xs :as v] (seq v)]
      (if v
        (cond
          (integer? x) (recur max-w aligns (long (+ x offset)) xs)
          (= :break x) (recur max-w aligns (long (first aligns)) xs)
          (= :align x) (recur max-w (cons offset aligns) offset xs)
          (= :align-indent x) (recur max-w (cons (inc offset) aligns) offset xs)
          (= :dealign x) (recur max-w (next aligns) offset xs)
          :else
          (let [rel-o (-relative-offset x)
                rel-w (-relative-width x)]
            (if (and (>= rel-o 0)
                     (>= rel-w 0))
              (recur (maxl (+ offset rel-w) max-w)
                     aligns
                     (long (+ offset rel-o))
                     xs)
              -1)))
        max-w)))
  (next-offset [v offset]
    (loop [aligns [offset]
           offset offset
           [x & xs :as v] (seq v)]
      (if v
        (cond
          (integer? x) (recur aligns (long (+ x offset)) xs)
          (= :break x) (recur aligns (first aligns) xs)
          (= :align x) (recur (cons offset aligns) offset xs)
          (= :align-indent x) (recur (cons (inc offset) aligns) offset xs)
          (= :dealign x) (recur (next aligns) offset xs)
          :else (recur aligns (next-offset x offset) xs))
        offset)))
  (width [v offset]
    (loop [max-w offset
           aligns [offset]
           offset offset
           [x & xs :as v] (seq v)]
      (if v
        (cond
          (integer? x) (recur max-w aligns (long (+ x offset)) xs)
          (= :break x) (recur max-w aligns (first aligns) xs)
          (= :align x) (recur max-w (cons offset aligns) offset xs)
          (= :align-indent x) (recur max-w (cons (inc offset) aligns) offset xs)
          (= :dealign x) (recur max-w (next aligns) offset xs)
          :else (recur (maxl max-w (width x offset))
                       aligns
                       (next-offset x offset)
                       xs))
        (maxl max-w offset))))
  (line-breaks [v]
    (loop [breaks 0
           [x & xs :as v] (seq v)]
      (if v
        (cond
          (= :break x) (recur (inc breaks) xs)
          (keyword? x) (recur breaks xs)
          (integer? x) (recur breaks xs)
          :else (recur (long (+ breaks (line-breaks x))) xs))
        breaks))))

(defn- not-relative [_] -1)

(defn- with-layout-meta [v rel-o rel-w breaks]
  (let [m (transient {`line-breaks (fn [_] breaks)})
        m (if (>= 0 rel-o)
            (assoc! m
                    `-relative-offset (fn [_] rel-o)
                    `next-offset (fn [_ offset] (+ rel-o offset)))
            (assoc! m `-relative-offset not-relative))
        m (if (>= 0 rel-w)
            (assoc! m
                    `-relative-width (fn [_] rel-w)
                    `width (fn [_ offset] (+ offset rel-w)))
            (assoc! m `-relative-width not-relative))]
    (with-meta v (persistent! m))))

(defn create [v]
  {:pre [(sequential? v)]}
  (let [v (if (vector? v) v (vec v))
        rel-o (-relative-offset v)
        rel-w (if (>= rel-o 0) (-relative-width v) -1)
        breaks (line-breaks v)]
    (with-layout-meta v rel-o rel-w breaks)))

(defrecord Comment [text])

(extend-protocol ILayout
  Comment
  (-flatten [comment] [comment])
  (-relative-offset [_] 0)
  (-relative-width [_] 0)
  (next-offset [_ offset] offset)
  (width [offset] offset)
  (line-breaks [_] 1))

(defn comment [text]
  (->Comment text))

(defrecord RawLine [content])

(extend-protocol ILayout
  RawLine
  (-flatten [raw] [raw])
  (-relative-offset [_] -1)
  (-relative-width [_] -1)
  (next-offset [^RawLine raw _] (count (.-content raw)))
  (width [^RawLine raw _] (count (.-content raw)))
  (line-breaks [_] 0))

(defn render [layout]
  (let [lines
        (loop [pending-space 0
               ^StringBuilder s (StringBuilder.)
               lines '()
               aligns '(0)
               [x & xs] (-flatten layout)]
          (if x
            (cond
              (integer? x) (recur (long (+ pending-space x)) s lines aligns xs)
              (string? x) (do (.append s (spaces pending-space))
                              (.append s x)
                              (recur 0 s lines aligns xs))
              (instance? Comment x) (recur (long (first aligns)) (StringBuilder.) (cons [s (:text x) pending-space] lines) aligns xs)
              (instance? RawLine x) (let [content ^String (:content x)]
                                      (.append s content)
                                      (recur 0 s lines aligns xs))
              (vector? x) (recur pending-space s lines aligns (concat x xs))
              (= :break x) (recur (long (first aligns)) (StringBuilder.) (cons [s nil pending-space] lines) aligns xs)
              (= :align x) (recur pending-space s lines (cons (+ pending-space (.length s)) aligns) xs)
              (= :align-indent x) (recur pending-space s lines (cons (+ 1 pending-space (.length s)) aligns) xs)
              (= :dealign x) (recur pending-space s lines (next aligns) xs))
            (cons [s nil pending-space] lines)))
        trailing-comment-lines
        (filter #(and (second %)
                      (pos? (.length ^StringBuilder (first %))))
                lines)
        comment-alignment
        (reduce #(maxl %1 (+ 2 (.length ^StringBuilder (first %2)))) 0 trailing-comment-lines)]
    (loop [contents '()
           [[^StringBuilder content comment alignment :as x] & xs] lines]
      (if x
        (let [n (.length content)
              has-content? (pos? n)]
          (recur (cond
                   (and has-content? comment) (cons (str (.toString content)
                                                         (spaces (- comment-alignment n))
                                                         comment)
                                                    contents)
                   has-content? (cons (.toString content) contents)
                   comment (cons (str (spaces alignment) comment) contents)
                   :else contents)
                 xs))
        (string/join "\n" contents)))))

(defn token [^String s]
  s)

(defn multiline [lines]
  {:pre [(sequential? lines)
         (every? #(and (boolean? (:absolute %))
                       (string? (:content %)))
                 lines)]}
  (case (count lines)
    1 (:content (first lines))
    (loop [lo (transient [:align (:content (first lines))])
           [{:keys [absolute content] :as x} & xs] (next lines)]
      (if x
        (recur (conj! (conj! lo :break) (if absolute (->RawLine content) content)) xs)
        (-> (conj! lo :dealign)
            (persistent!)
            (create))))))

(defn builder [offset target-width alternative]
  (LayoutBuilder. offset target-width alternative))

(defn append! [^LayoutBuilder builder item]
  (when (and item builder)
    (cond
      (= :break item)
      (doto builder
        (.addLineBreaks 1)
        (.setOffset (first (.-aligns builder)))
        (.addItem item))
      (= :align item)
      (doto builder
        (.align (.-offset builder))
        (.addItem item))
      (= :align-indent item)
      (doto builder
        (.align (inc (.-offset builder)))
        (.addItem item))
      (= :dealign item)
      (doto builder
        (.dealign)
        (.addItem item))
      (integer? item)
      (doto builder
        (.setOffset (+ (.-offset builder) item))
        (.addItem item))
      :else
      (let [rel-o (-relative-offset item)
            rel-w (if (>= rel-o 0) (-relative-width item) -1)
            offset (if (>= rel-o 0)
                     (+ (.-offset builder) rel-o)
                     (next-offset item (.-offset builder)))]
        (doto builder
          (.setOffset offset)
          (.updateRelativeValues rel-o rel-w)
          (.addLineBreaks (line-breaks item))
          (.addItem item))))))

(defn append-node! [^LayoutBuilder builder node formatter]
  (if node
    (append! builder (formatter node (.-offset builder) (.-targetWidth builder) (.-alternative builder)))
    builder))

(defn build! [^LayoutBuilder builder]
  (let [v (persistent! (.-contents builder))
        rel-o (.-relativeOffset builder)
        rel-w (.-relativeWidth builder)
        breaks (.-lineBreaks builder)]
    (with-layout-meta v rel-o rel-w breaks)))

(defn append-node [^LayoutBuilder builder maybe-node formatter]
  (if maybe-node
    (when-some [item (formatter maybe-node (.-offset builder) (.-targetWidth builder) (.-alternative builder))]
      (let [rel-o (-relative-offset item)
            rel-w (if (>= rel-o 0) (-relative-width item) -1)
            offset (if (>= rel-o 0)
                     (+ (.-offset builder) rel-o)
                     (next-offset item (.-offset builder)))]
        (set! (.-relativeOffset builder) rel-o)
        (set! (.-relativeWidth builder) (if (>= rel-w 0)
                                          (maxl rel-w (.-relativeWidth builder))
                                          -1))
        (set! (.-offset builder) offset)))
    builder))
