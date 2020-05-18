(ns imo.formatting.core
  (:require [imo.util :refer [node?]])
  (:import (java.io Writer)
           (imo Layout)
           (imo RawLayout InlineLayout AlignedLayout AltLayout SelectLayout)))

(defn- bits->props [bits]
  {:max-shrinkable-width       (Layout/maxShrinkableWidth bits)
   :first-line-width           (Layout/firstLineWidth bits)
   :last-line-width            (Layout/lastLineWidth bits)
   :max-width                  (Layout/maxWidth bits)
   :shrink-precedence          (Layout/shrinkPrecedence bits)
   :first-line-shrinkable?     (Layout/isFirstLineShrinkable bits)
   :last-line-shrinkable?      (Layout/isLastLineShrinkable bits)
   :last-line-offset-absolute? (Layout/isLastLineOffsetAbsolute bits)
   :multiline?                 (Layout/isMultiline bits)})

(defmethod print-method Layout [^Layout layout ^Writer writer]
  (let [kind (.kind layout)
        props (merge (bits->props (.-bits layout))
                     (.inspectProps layout))
        children (.inspectChildren layout)
        node (if (and (= :raw kind)
                      (not (:multiline? props)))
               [:raw (first children)]
               (into [kind props] children))
        s (pr-str node)]
    (.write writer ^String s)))

(defn- shrink*
  [layout
   target-width
   target-precedence
   width-factor]
  (let [width (int (Math/ceil (* target-width width-factor)))]
    (Layout/shrinkUntil layout 0 0 width target-precedence)))

(declare raw inline align alt dynamic |* | -| -|* space layout? layout)

(defn- group-line-n
  "General case for grouped line items: `a b c d`. If the line
   is too wide, it will be narrowed by dropping the latest item
   into next line:
   ```
   a b c
   d
   ```
  "
  [items]
  {:pre [(vector? items)
         (seq items)]}
  (if (> (count items) 1)
    (alt (inline (interpose space items))
         (fn group-line-n-alt []
           (align [(group-line-n (pop items))
                   (-| (peek items))])))
    (first items)))

(defn- group-line-1
  "Special case for first grouped line. Instead of aligning the dropped
   item with the first line, the dropped item will be indended:
   ```
   a b c
    d
   ```
  "
  [items]
  {:pre [(vector? items)
         (seq items)]}
  (if (> (count items) 1)
    (alt (inline (interpose space items))
         (fn group-line-1-alt []
           (align [(group-line-1 (pop items))
                   (-| (peek items))])))
    (first items)))

(defn- coerce-layout [x]
  (cond
    (layout? x) x
    (node? x) (layout x)
    (nil? x) nil
    (string? x) (raw x)
    :else (throw (IllegalArgumentException. (str "Can't coerce to layout: " (pr-str x))))))


;;
;;

(def ^:const BEST
  "The best layout that we can output if it fits to
   the given target width."
  32)

(def ^:const BETTER
  "Layout that is not as good as primary but will
   still produce pretty good formatting"
  16)

(def ^:const GOOD
  "Layout that is okay'ish and will produce decent
   formatting but not optimal"
  8)

(def ^:const WORSE
  "Layout that will produce mediocre formatting
   that should be avoided if possible"
  4)

(def ^:const WORST
  "Layout that produce highly unoptimal formatting
   that should be avoided at any cost"
  2)

(defn layout?
  "Returns boolean whether the given value is a valid
   layout or not"
  {:inline (fn [x] `(instance? Layout ~x))}
  [x]
  (instance? Layout x))

(defn multiline?
  "Returns boolean whether the given output has multiple
   lines or not"
  [^Layout layout]
  {:pre [(layout? layout)]}
  (Layout/isMultiline (.-bits layout)))

(defn raw
  "Produces raw output from the given string"
  ([^String s] (raw s false))
  ([^String s ^Boolean may-contain-new-lines?]
   {:pre [(string? s)
          (or may-contain-new-lines? (not (re-find #"\n" s)))]}
   (RawLayout/create s may-contain-new-lines?)))

(defn inline
  "Produces constant inline layout for the given children:
   all children are arranged sequentially after each other

   Example:
   ```clj
   (inline (raw \"[\") (raw \"tsers\") (raw \"]\"))
   ; outputs: [tsers]
   ```"
  [children]
  {:pre [(or (sequential? children)
             (nil? children))]}
  (InlineLayout/create (seq children) nil))

(defn align
  "Produces constant aligned layout for the given children:
   each children is started at the new line so that the start
   offset of each child is same as the first child's

   Example:
   ```clj
   (align (raw \"foo\") (raw \"bar\"))
   ; outputs: foo
   ;          bar
   ```
   "
  [children]
  {:pre [(or (sequential? children)
             (nil? children))]}
  (AlignedLayout/create (seq children) nil))

(defn alt
  "Produces layout that uses the given primary layout as a first
   choice but offers also a secondary layout in case if the primary
   one is too wide.

   Secondary layout gets evaluated lazily only on-demand, receiving
   the current children as the first (and oly) parameter. Note that
   those children may not be same as the original children because
   they might be shrinked before evaluating the secondary layout.
   However, the count of the children is quaranteed to remain same
   for both primary and secondary layout.

   Using `GOOD` as default precedence for the secondary layout.
  "
  ([^Layout primary secondary] (alt primary secondary GOOD))
  ([^Layout primary secondary precedence]
   {:pre [(layout? primary)
          (ifn? secondary)
          (pos-int? precedence)]}
   (AltLayout/create primary secondary precedence nil)))

(defn select
  "Produces layout that uses the given selector function for each
   shrink to procuce a new layout.

   Selector function signature is:
   `(children, target-width, target-precedence) => [output, children]`

   Children are accumulated over shrink steps so that selector
   receives children from the previous step.
   "
  [selector children]
  {:pre [(ifn? selector)
         (sequential? children)
         (every? layout? children)]}
  (SelectLayout/create selector children Integer/MAX_VALUE BEST nil))

(defn group
  "Default layout for a 'group of items' so that items get dropped
   into next lines starting from the end of the group. Dropped items
   will be indended.

   ```
   a b c d

   a b c
    d

   a b
    c
    d
   ```"
  [layouts]
  {:pre (every? layout? layouts)}
  (when (seq layouts)
    (loop [full-lines nil
           current (transient [(first layouts)])
           [l & rem] (next layouts)]
      (if (some? l)
        (if (multiline? l)
          ; Encountered multiline layout: it means that we must
          ; split the group start next item from new line. Line
          ; items may also be splitted later and in that case, the
          ; first line has a little bit different semantics, thus
          ; sparating first-line and nth-line cases
          (let [line-items (persistent! current)]
            (if (nil? full-lines)
              (recur (transient [(group-line-1 line-items)])
                     (transient [])
                     rem)
              (recur (conj! full-lines (group-line-n line-items))
                     (transient [])
                     rem)))
          (recur full-lines (conj! current l) rem))
        ; End of grouped layouts
        (let [last-line (persistent! current)
              full-lines (some-> full-lines (persistent!))]
          (if full-lines
            (| (first full-lines)
               (-|* (conj (subvec full-lines 1) (group-line-n last-line))))
            (group-line-1 last-line)))))))

(defn fit [^Layout layout width]
  "Tries to fit layout to the target given target width"
  {:pre [(layout? layout)
         (pos-int? width)]}
  (-> layout
      (shrink* width BEST 1.0)
      (shrink* width BETTER 1.1)
      (shrink* width GOOD 1.2)
      (shrink* width WORSE 1.3)
      (shrink* width WORST 1.5)))

(defn render
  "Renders the given layout to a string"
  [^Layout layout]
  {:pre [(layout? layout)]}
  (let [sb (StringBuilder.)]
    (.print layout sb 0)
    (.toString sb)))

(def space
  "Pre-calculated constant output for spaces"
  (raw " "))

(defmulti layout
  "Multi-function for layout creation for a single AST node"
  first)

(defmethod layout :default [node]
  (throw (RuntimeException. (str "No node layout implementation for " (first node)))))

(defn |*
  "Convenience function for producing an aligned layout
   from a sequence of lines"
  [lines]
  (-> (keep #(cond
               (node? %) (layout %)
               (vector? %) (group (keep coerce-layout %))
               (sequential? %) (keep coerce-layout %)
               :else %)
            lines)
      (align)))

(defn |
  "Convenience function for producing an aligned layout
   from the given variadic lines"
  [& lines]
  (|* lines))

(defn -|*
  "Convenience function for producing an aligned and
   indended layout from a sequence of lines"
  [lines]
  (inline [space (|* lines)]))

(defn -|
  "Convenience function for producing an aligned and
   indended layout from the given variadric lines"
  [& lines]
  (-|* lines))
