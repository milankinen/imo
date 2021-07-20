(ns imo.layout.builder
  (:require [imo.layout.core :as l])
  (:import (imo LayoutBuilder)))

(defn builder [offset target-width alternative]
  (LayoutBuilder. offset target-width alternative nil nil))

(defn coll-builder [offset target-width alternative begin-chars end-chars indent?]
  (let [prefix [begin-chars (if indent? :align-indent :align)]
        suffix [end-chars :dealign]]
    (LayoutBuilder. offset target-width alternative prefix suffix)))

(defn offset [^LayoutBuilder builder]
  (.-offset builder))

(defn target-width [^LayoutBuilder builder]
  (.-targetWidth builder))

(defn alternative [^LayoutBuilder builder]
  (.-alternative builder))

(defn add! [^LayoutBuilder builder item]
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
      (let [offset (.-offset builder)
            rel-o (l/-relative-offset item)
            rel-w (if (>= rel-o 0) (l/-relative-width item) -1)
            offset (if (>= rel-o 0)
                     (+ offset rel-o)
                     (l/next-offset item offset))]
        (doto builder
          (.setOffset offset)
          (.updateRelativeValues rel-o rel-w)
          (.addLineBreaks (l/line-breaks item))
          (.addItem item))))))

(defn append-node! [^LayoutBuilder builder node formatter]
  (if node
    (if (or (integer? node)
            (keyword? node)
            (string? node))
      (add! builder node)
      (add! builder (formatter node (.-offset builder) (.-targetWidth builder) (.-alternative builder))))
    builder))

(defn append-node-one-line! [^LayoutBuilder builder node formatter]
  (if node
    (if (or (integer? node)
            (keyword? node)
            (string? node))
      (add! builder node)
      (when-some [item (formatter node (.-offset builder) (.-targetWidth builder) (.-alternative builder))]
        (when (or (zero? (.-alternative builder))
                  (zero? (l/line-breaks item)))
          (add! builder item))))
    builder))

(defn build! [^LayoutBuilder builder]
  (when builder
    (let [v (.persistent builder)
          rel-o (.-relativeOffset builder)
          rel-w (.-relativeWidth builder)
          breaks (.-lineBreaks builder)]
      (l/-with-layout-meta v rel-o rel-w breaks))))
