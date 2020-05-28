(ns imo.analysis.spec
  (:refer-clojure :exclude [* + rem seq])
  (:require [clojure.string :as string]
            [imo.util :refer [start-of end-of]])
  (:import (clojure.lang IDeref IFn Indexed)
           (imo AnalysisException)))

; Accumulated value, "right"
(defrecord Acc [ctx remaining result]
  Indexed
  (nth [_ i]
    (case i
      0 ctx
      1 remaining
      2 result
      nil))
  (nth [_ i not-found]
    (case i
      0 ctx
      1 remaining
      2 result
      not-found)))

; Error output, "left"
(defrecord Err [^IFn get-position ^String message])

(definterface Spec
  (name [])
  (run [node input last?]))

(defn error
  "Constructs error that can be outputted from specs to
   indicate that input data does not satisfy the spec"
  [parent node message]
  (->Err #(if (some? node)
            (start-of node)
            (end-of parent))
         message))

(defn error?
  "Returns boolean whether the given value is spec error or not"
  {:inline (fn [x] `(instance? Err ~x))}
  [maybe-error]
  (instance? Err maybe-error))

(defn spec?
  "Returns boolean whether the given value is spec or not"
  [maybe-spec]
  (instance? Spec maybe-spec))

(defn- spec->name [^Spec spec]
  {:pre [(spec? spec)]}
  (.name spec))

;; Core specs

(defn check
  "Performs check to the current accumulated value. Check function
   should return spec error or nil depending on whether the accumulated
   value (state) is valid or not"
  [f]
  {:pre [(ifn? f)]}
  (reify Spec
    (name [_] nil)
    (run [_ parent input _]
      (if-let [error (f parent input)]
        (do (assert (error? error))
            error)
        input))))

(defn custom
  "Creates custom spec from the given function having a
   signature of `(parent-node, acc) -> acc | error`"
  [f]
  {:pre [(ifn? f)]}
  (reify Spec
    (name [_] nil)
    (run [_ parent input _]
      (let [output (f parent input)]
        (assert (or (error? output) (= 3 (count output))))
        output))))

(defn ? [^Spec spec]
  "Matches the next node using the spec. If spec returns an
   error, puts `nil` to the result, otherwise uses the output
   from spec."
  {:pre [(spec? spec)]}
  (reify Spec
    (name [_] (spec->name spec))
    (run [_ parent input last?]
      (let [output (.run spec parent input last?)]
        (if (error? output)
          (if-not (and last? (not-empty (.-remaining ^Acc input)))
            (Acc. (.-ctx ^Acc input) (.-remaining ^Acc input) (conj! (.-result ^Acc input) nil))
            output)
          output)))))

(defn * [^Spec spec]
  "Matches remaining nodes until spec returns error and suppresses
   that error."
  {:pre [(spec? spec)]}
  (reify Spec
    (name [_] (spec->name spec))
    (run [_ parent input last?]
      (loop [output input]
        (let [out' (.run spec parent output last?)]
          (if (error? out')
            (if (and last? (not-empty (.-remaining ^Acc output)))
              out'
              output)
            (recur out')))))))

(defn + [^Spec spec]
  "Matches remaining nodes until spec returns true. If there is at least
   one match, the last error will be suppressed. Otherwise returns the
   spec error"
  {:pre [(spec? spec)]}
  (reify Spec
    (name [_] (spec->name spec))
    (run [_ parent input last?]
      (let [output (.run spec parent input last?)]
        (if-not (error? output)
          (loop [output output]
            (let [out' (.run spec parent output last?)]
              (if (error? out')
                (if (and last? (not-empty (.-remaining ^Acc output)))
                  out'
                  output)
                (recur out'))))
          output)))))

(defn named
  "Renames the given spec"
  [spec-name ^Spec spec]
  {:pre [(string? spec-name)
         (spec? spec)]}
  (reify Spec
    (name [_] spec-name)
    (run [_ parent input last?]
      (.run spec parent input last?))))

(defn seq
  "Creates a 'sequence' spec that evaluates every one of the
   given specs once or bails out on first errorous spec"
  [& specs]
  {:pre [(every? #(or (spec? %) (nil? %)) specs)]}
  (let [specs (doall (filter some? specs))]
    (assert (not-empty specs))
    (reify Spec
      (name [_] (first (keep spec->name specs)))
      (run [_ parent input last?]
        (loop [output input
               [spec & specs] specs]
          (if (some? spec)
            (let [last? (and last? (empty? specs))
                  out' (.run ^Spec spec parent output last?)]
              (if-not (error? out')
                (recur out' specs)
                out'))
            output))))))

(defn choose
  "Takes a pair of predicates and specs and runs the predicate (having
   a signature `(ctx node) -> bool`) to the next remaining node and
   select spec for the first successful predicate. Fallbacks to the
   default value (spec without predicate) or if there is no default,
   returns an error."
  [& preds+specs]
  {:pre [(pos-int? (count preds+specs))]}
  (let [default (when (odd? (count preds+specs))
                  (last preds+specs))
        preds+specs (if (odd? (count preds+specs))
                      (doall (butlast preds+specs))
                      (doall preds+specs))
        specs (mapv second (partition-all 2 preds+specs))
        err-msg (delay (str "expected one of: " (string/join ", " (sort (set (keep spec->name specs))))))]
    (assert (every? spec? specs))
    (assert (or (nil? default) (spec? default)))
    (reify Spec
      (name [_] (str "one of: " (string/join ", " (sort (set (keep spec->name specs))))))
      (run [_ parent input last?]
        (let [ctx (.-ctx ^Acc input)
              tested (first (.-remaining ^Acc input))]
          (loop [[pred spec & xs] preds+specs]
            (if (some? pred)
              (if (pred ctx tested)
                (.run ^Spec spec parent input last?)
                (recur xs))
              (if (some? default)
                (.run ^Spec default parent input last?)
                (error parent tested @err-msg)))))))))

(defn recursive
  "Creates recursive spec from the given derefable"
  [spec]
  {:pre [(instance? IDeref spec)]}
  (reify Spec
    (name [_]
      (spec->name @spec))
    (run [_ parent input last?]
      (assert (spec? @spec))
      (.run ^Spec @spec parent input last?))))

(defn fail
  "Creates spec that always fails with the given message"
  [message]
  {:pre [(string? message)]}
  (reify Spec
    (name [_] nil)
    (run [_ parent input _]
      (error parent (first (.-remaining ^Acc input)) message))))

(defn run
  "Runs the given spec againt the children of the given node
   and returns node annotated by the spec"
  [^Spec spec ctx [node-type & children :as node]]
  (let [input (Acc. ctx children (transient [node-type]))
        output (.run spec node input true)]
    (if (error? output)
      (throw (AnalysisException. (:get-position output) (:message output)))
      (let [ctx' (.-ctx ^Acc output)
            rem (.-remaining ^Acc output)
            res (.-result ^Acc output)]
        (if-let [extra (first rem)]
          (throw (AnalysisException. #(start-of extra) "no more forms expected"))
          [ctx' (persistent! res)])))))

(defn- composed-analyzer [analyzer post-analyzer]
  (if (ifn? post-analyzer)
    (fn analyzer+post-analyzer [ctx node]
      (let [[ctx' node'] (analyzer ctx node)]
        (post-analyzer ctx' node')))
    analyzer))

(defn as-analyzer
  "Converts spec or analyzer with optional post analyzer
   into a composed analyzer function `(ctx, node) -> node`"
  ([^Spec spec-or-analyzer] (as-analyzer spec-or-analyzer nil))
  ([^Spec spec-or-analyzer post-analyzer]
   {:pre [(or (spec? spec-or-analyzer)
              (ifn? spec-or-analyzer))
          (or (nil? post-analyzer)
              (ifn? post-analyzer))]}
   (let [spec-analyzer (if (spec? spec-or-analyzer)
                         (fn analyzer-from-spec [ctx node]
                           (run spec-or-analyzer ctx node))
                         spec-or-analyzer)]
     (composed-analyzer spec-analyzer post-analyzer))))
