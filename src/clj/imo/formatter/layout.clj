(ns imo.formatter.layout
  (:refer-clojure :exclude [newline])
  (:require [imo.ast :as ast]
            [imo.logger :refer [vv vvv]])
  (:import (java.io Writer StringWriter)
           (java.util HashMap)))

;;
;; Output
;;

(defrecord Line [tokens length alignment])
(defrecord Output [lines line alignment stack])

(defn- write-token [token ^Writer w]
  (if (integer? token)
    (loop [n token]
      (when (pos-int? n)
        (.write ^Writer w " ")
        (recur (dec n))))
    (.write ^Writer w ^String token)))

(defn- write-line [line ^Writer w]
  (when (not-empty (:tokens line))
    (write-token (:alignment line) w)
    (doseq [t (reverse (:tokens line))]
      (write-token t w))))

(defn- write-out [output ^Writer w]
  (doseq [line (reverse (:lines output))]
    (write-line line w)
    (.write ^Writer w "\n"))
  (write-line (:line output) w))

(defn- newline [alignment]
  {:pre [(nat-int? alignment)]}
  (->Line (list) alignment alignment))

(defn output? [x]
  (instance? Output x))

(defn line? [x]
  (instance? Line x))

(defn token? [x]
  (or (string? x)
      (nat-int? x)))

(defn length [t]
  {:pre [(token? t)]}
  (cond (integer? t) t
        (string? t) (.length ^String t)))

(defn empty-output
  "Creates a new empty output"
  []
  (map->Output {:lines     (list)
                :line      (newline 0)
                :alignment 0
                :stack     (list 0)}))

(defn push-align
  "TODO"
  [output]
  {:pre [(output? output)]}
  (let [a (get-in output [:line :length])]
    (assoc output :alignment a :stack (cons a (:stack output)))))

(defn pop-align
  "TODO"
  [output]
  {:pre [(output? output)]}
  (let [[_ & stack] (:stack output)
        a (first stack)]
    (assert (some? a))
    (assoc output :alignment a :stack stack)))

(defn push-token
  "Pushes token to the given output"
  [output token]
  {:pre [(output? output)
         (token? token)]}
  (if (integer? token)
    (if (pos-int? token)
      (let [l (:line output)
            l' (assoc l :length (+ (:length l) token)
                        :tokens (cons token (:tokens l)))]
        (assoc output :line l'))
      output)
    (let [l (:line output)
          t-len (.length ^String token)
          l' (assoc l :length (+ (:length l) t-len)
                      :tokens (cons token (:tokens l)))]
      (assoc output :line l'))))

(defn push-newline
  "TODO"
  [output]
  (let [l (newline (:alignment output))
        ls (cons (:line output) (:lines output))]
    (assoc output :lines ls :line l)))

(defn column
  "Returns the current column of the given output"
  [output]
  {:pre [(output? output)]}
  (inc (get-in output [:line :length])))

(defn line
  "Returns the current line of the given output"
  [output]
  {:pre [(output? output)]}
  (inc (count (:lines output))))

(defn to-src
  "Returns source string of the given output"
  [out]
  {:pre [(output? out)]}
  (let [w (StringWriter.)]
    (write-out out w)
    (.toString w)))

;;
;; Layout primitives
;;

(declare as-layout)
(declare select-best)

(def ^:dynamic ^:private *max-width* nil)
(def ^:dynamic ^:private *cache* nil)

(defn- fits-current-line? [out token]
  (let [col (column out)
        len (length token)]
    (<= (+ len col) *max-width*)))

(defmulti -node-layout (fn [node] (first node)))

(defrecord LayoutParams
  [^Boolean inline
   ^Long force])

(defn- layout-params [force-level]
  (->LayoutParams false force-level))

(defprotocol Layout
  (-ws-only? [_])
  (-fit [_ ^LayoutParams params out]))

(defrecord TokenLayout [token]
  Layout
  (-ws-only? [_] (integer? token))
  (-fit [_ params out]
    (when (or (fits-current-line? out token)
              (pos-int? (:force params)))
      [(push-token out token)])))

(defrecord LineLayout [items]
  Layout
  (-ws-only? [_] false)
  (-fit [_ params out]
    (loop [[it & rem] items
           result [out]]
      (if (some? it)
        (when-let [res (seq (mapcat #(-fit it params %) result))]
          (recur rem res))
        result))))

(defrecord InlineLayout [inner]
  Layout
  (-ws-only? [_] (-ws-only? inner))
  (-fit [_ params out]
    (-fit inner (assoc params :inline true) out)))

(defrecord AlignedLayout [items]
  Layout
  (-ws-only? [_] false)
  (-fit [_ params out]
    (when (or (false? (:inline params))
              (pos-int? (:force params)))
      (loop [[it & rem] items
             result [(push-align out)]]
        (when-let [res (seq (mapcat #(-fit it params %) result))]
          (if (not-empty rem)
            (recur rem (map push-newline res))
            (map pop-align res)))))))

(defrecord AltLayout [alternatives]
  Layout
  (-ws-only? [_] false)
  (-fit [_ params out]
    (seq (mapcat #(-fit % params out) alternatives))))

(defrecord BestLayout [inner]
  Layout
  (-ws-only? [_] (-ws-only? inner))
  (-fit [_ params out]
    (let [result (-fit inner params out)]
      (vvv "Selecting best of " (count result) " intermediate outputs")
      [(select-best result)])))

(defn- has-content? [layouts]
  (true? (some #(not (-ws-only? %)) layouts)))

(defn- token-layout [token]
  (->TokenLayout token))

(defn- node-layout [node]
  (let [id (:id (meta node))
        cache *cache*
        lo (if (and (some? id) (some? cache))
             (or (.get ^HashMap cache id)
                 (binding [*cache* (HashMap.)]
                   (let [lo (-node-layout node)]
                     (.put ^HashMap cache id lo)
                     lo)))
             (binding [*cache* (HashMap.)]
               (-node-layout node)))]
    (assert (seq lo) (str "No layout defined for node: " node))
    lo))

(defn- alt-layout [alternatives]
  (let [layouts (keep as-layout alternatives)]
    (when (has-content? layouts)
      (case (count layouts)
        0 nil
        1 (first layouts)
        (->AltLayout layouts)))))

(defn- line-layout [items]
  (let [layouts (keep as-layout items)]
    (when (has-content? layouts)
      (case (count layouts)
        0 nil
        1 (first layouts)
        (->LineLayout layouts)))))

(defn- aligned-layout [items]
  (let [layouts (keep as-layout items)]
    (when (has-content? layouts)
      (case (count layouts)
        0 nil
        1 (first layouts)
        (->AlignedLayout layouts)))))

(defn- as-layout [x]
  (cond
    (nil? x) nil
    (satisfies? Layout x) x
    (ast/node? x) (node-layout x)
    (token? x) (token-layout x)
    (vector? x) (line-layout x)
    true (throw (AssertionError. (str "Unsupported layout: " x)))))

(defn inline [items]
  (line-layout items))

(defn align [items]
  (aligned-layout items))

(defn ! [& items]
  (inline items))

(defn | [& items]
  (align items))

(defn $ [& alternatives]
  (alt-layout alternatives))

(defn best [layout]
  (some-> (as-layout layout)
          (->BestLayout)))


;;
;; Output fitting and scoring
;;

(defn- max-width [target-width]
  (int (Math/ceil (* 1.2 target-width))))

(defn- select-best [result]
  (first result))

(defn fit [layout config out]
  {:pre [(map? config)
         (output? out)]}
  (let [lo (as-layout layout)]
    (assert (some? lo) (str "Can't fit layout: " layout))
    #_(clojure.pprint/pprint lo)
    (binding [*max-width* (max-width (:width config))]
      (loop [force 0]
        (if (<= force 10)
          (if-let [res (-fit lo (layout-params force) out)]
            (do (vv "Layout fitting produced " (count res) " outputs, selecting best")
                (select-best res))
            (recur (inc force)))
          (throw (AssertionError. "Fitting failed")))))))
