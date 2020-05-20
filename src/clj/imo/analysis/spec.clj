(ns imo.analysis.spec
  (:refer-clojure :exclude [* + rem seq])
  (:require [clojure.string :as string]
            [imo.analysis.core :refer [default-node-analyzer
                                       analyze-with
                                       analysis-ex
                                       get-node-type
                                       get-literal-content]]
            [imo.util :refer [start-of end-of]])
  (:import (clojure.lang IDeref)))

;; Utils

; These getters and setters are bit verbose but they will be used
; exhaustively during the analysis stage so they need to be as
; fast as possible, hence direct field access and inlining so that
; we can bypass all Clojure runtime stuff and emit high-performance
; bytecode for the core specs

(defrecord State [ctx remaining result])

(defn ^{:inline (fn [s] `(.-ctx ~s))}
  state->ctx [^State s]
  (.-ctx s))

(defn ^{:inline (fn [s] `(.-remaining ~s))}
  state->remaining [^State s]
  (.-remaining s))

(defn ^{:inline (fn [s] `(first (.-remaining ~s)))}
  state->next-node [^State s]
  (first (.-remaining s)))

(defn ^{:inline (fn [s] `(.-result ~s))}
  state->result [^State s]
  (.-result s))

(defn ^{:inline (fn [s v]
                  `(let [s# ~s]
                     (State. ~v (state->remaining s#) (state->result s#))))}
  with-ctx [^State s v]
  (State. v (state->remaining s) (state->result s)))

(defn ^{:inline (fn [s v]
                  `(let [s# ~s]
                     (State. (state->ctx s#) ~v (state->result s#))))}
  with-remaining [^State s v]
  (State. (state->ctx s) v (state->result s)))

(defn ^{:inline (fn [s v]
                  `(let [s# ~s]
                     (State. (state->ctx s#) (state->remaining s#) ~v)))}
  with-result [^State s v]
  (State. (state->ctx s) (state->remaining s) v))

(definterface Spec
  (name [])
  (run [node input last?]))

(defrecord Err [position message])

(defn error [parent node message]
  (let [position (if (some? node)
                   (start-of node)
                   (end-of parent))]
    (->Err position message)))

(defn ^{:inline (fn [x] `(instance? Err ~x))}
  error? [x]
  (instance? Err x))

(defn ^{:inline (fn [x] `(instance? Spec ~x))}
  spec? [x]
  (instance? Spec x))

(defn spec->name [^Spec spec]
  {:pre [(spec? spec)]}
  (.name spec))

;; Core specs

(defn check [f]
  {:pre [(ifn? f)]}
  (reify Spec
    (name [_] nil)
    (run [_ parent input _]
      (if-let [error (f parent input)]
        (do (assert (error? error))
            error)
        input))))

(defn custom [f]
  {:pre [(ifn? f)]}
  (reify Spec
    (name [_] nil)
    (run [_ parent input _]
      (let [output (f parent input)]
        (assert (or (error? output) (= 3 (count output))))
        output))))

(defn ? [^Spec spec]
  {:pre [(spec? spec)]}
  (reify Spec
    (name [_] (spec->name spec))
    (run [_ parent input last?]
      (let [output (.run spec parent input last?)]
        (if (error? output)
          (if-not (and last? (not-empty (state->remaining ^State input)))
            (with-result ^State input (conj! (state->result ^State input) nil))
            output)
          output)))))

(defn * [^Spec spec]
  {:pre [(spec? spec)]}
  (reify Spec
    (name [_] (spec->name spec))
    (run [_ parent input last?]
      (loop [output input]
        (let [out' (.run spec parent output last?)]
          (if (error? out')
            (if (and last? (not-empty (state->remaining ^State output)))
              out'
              output)
            (recur out')))))))

(defn + [^Spec spec]
  {:pre [(spec? spec)]}
  (reify Spec
    (name [_] (spec->name spec))
    (run [_ parent input last?]
      (let [output (.run spec parent input last?)]
        (if-not (error? output)
          (loop [output output]
            (let [out' (.run spec parent output last?)]
              (if (error? out')
                (if (and last? (not-empty (state->remaining ^State output)))
                  out'
                  output)
                (recur out'))))
          output)))))

(defn named [spec-name ^Spec spec]
  {:pre [(string? spec-name)
         (spec? spec)]}
  (reify Spec
    (name [_] spec-name)
    (run [_ parent input last?]
      (.run spec parent input last?))))

(defn seq [& specs]
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

(defn choose [& preds+specs]
  {:pre [(pos-int? (count preds+specs))]}
  (let [default (when (odd? (count preds+specs))
                  (last preds+specs))
        preds+specs (if (odd? (count preds+specs))
                      (doall (butlast preds+specs))
                      (doall preds+specs))
        specs (mapv second (partition-all 2 preds+specs))]
    (assert (every? spec? specs))
    (assert (or (nil? default) (spec? default)))
    (reify Spec
      (name [_] (str "one of: " (string/join ", " (keep spec->name specs))))
      (run [this parent input last?]
        (let [ctx (state->ctx ^State input)
              tested (first (state->remaining ^State input))]
          (loop [[pred spec & xs] preds+specs]
            (if (some? pred)
              (if (pred ctx tested)
                (.run ^Spec spec parent input last?)
                (recur xs))
              (if (some? default)
                (.run ^Spec default parent input last?)
                (error parent tested (str "expected " (spec->name this)))))))))))

(defn recursive [spec]
  {:pre [(instance? IDeref spec)]}
  (reify Spec
    (name [_]
      (assert (spec? @spec))
      (spec->name @spec))
    (run [_ parent input last?]
      (assert (spec? @spec))
      (.run ^Spec @spec parent input last?))))

(defn fail [message]
  {:pre [(string? message)]}
  (reify Spec
    (name [_] nil)
    (run [_ parent input _]
      (error parent (state->next-node ^State input) message))))

(defn run [^Spec spec ctx [node-type & children :as node]]
  (let [input (State. ctx children (transient [node-type]))
        output (.run spec node input true)]
    (if (error? output)
      (throw (analysis-ex (:position output) (:message output)))
      (let [ctx' (state->ctx ^State output)
            rem (state->remaining ^State output)
            res (state->result ^State output)]
        (if-let [extra (first rem)]
          (throw (analysis-ex (start-of extra) "no more forms expected"))
          [ctx' (persistent! res)])))))

(defn- composed-analyzer [analyzer post-analyzer]
  (if (ifn? post-analyzer)
    (fn analyzer+post-analyzer [ctx node]
      (let [[ctx' node'] (analyzer ctx node)]
        (post-analyzer ctx' node')))
    analyzer))

(defn as-analyzer
  ([^Spec spec] (as-analyzer spec nil))
  ([^Spec spec post-analyzer]
   {:pre [(spec? spec)
          (or (nil? post-analyzer)
              (ifn? post-analyzer))]}
   (let [spec-analyzer (fn analyzer-from-spec [ctx node]
                         (run spec ctx node))]
     (composed-analyzer spec-analyzer post-analyzer))))

;; Primitive specs

(defn node [expected-type spec-name spec-or-analyzer]
  {:pre [(keyword? expected-type)
         (string? spec-name)
         (or (spec? spec-or-analyzer)
             (ifn? spec-or-analyzer))]}
  (let [analyzer (if (spec? spec-or-analyzer) (as-analyzer spec-or-analyzer) spec-or-analyzer)
        any-node? (= :any expected-type)]
    (reify Spec
      (name [_] spec-name)
      (run [_ parent input _]
        (let [ctx (state->ctx ^State input)
              rem (state->remaining ^State input)
              res (state->result ^State input)
              node (first rem)]
          (cond
            (nil? node)
            (->Err (end-of parent) (str "expected " spec-name))
            (or any-node? (= expected-type (get-node-type ctx node)))
            (let [[ctx' n'] (analyze-with analyzer ctx node)]
              (State. ctx' (next rem) (conj! res n')))
            :else (->Err (start-of node) (str "expected " spec-name " to be " expected-type))))))))

;; Shorthand specs

(defn- make-shorthand [expected-type analyzer]
  (fn shorthand-node-spec-factory
    ([spec-name] (shorthand-node-spec-factory spec-name nil))
    ([spec-name post-analyzer]
     {:pre [(string? spec-name)
            (or (nil? post-analyzer)
                (ifn? post-analyzer))]}
     (node expected-type spec-name (composed-analyzer analyzer post-analyzer)))))

(def any-node
  (make-shorthand :any default-node-analyzer))

(defn simple-symbol-node
  ([spec-name] (simple-symbol-node spec-name nil))
  ([spec-name post-analyzer]
   {:pre [(string? spec-name)
          (or (nil? post-analyzer)
              (ifn? post-analyzer))]}
   (let [analyzer (composed-analyzer default-node-analyzer post-analyzer)]
     (reify Spec
       (name [_] spec-name)
       (run [_ parent input _]
         (let [ctx (state->ctx ^State input)
               rem (state->remaining ^State input)
               res (state->result ^State input)
               node (first rem)]
           (cond
             (nil? node)
             (->Err (end-of parent) (str "expected " spec-name))
             (and (= :symbol (get-node-type ctx node))
                  (simple-symbol? (symbol (get-literal-content ctx node))))
             (let [[ctx' n'] (analyze-with analyzer ctx node)]
               (State. ctx' (next rem) (conj! res n')))
             :else (->Err (start-of node) (str "expected " spec-name " to be simple symbol")))))))))

(def symbol-node
  (make-shorthand :symbol default-node-analyzer))

(def keyword-node
  (make-shorthand :keyword default-node-analyzer))

(def list-node
  (make-shorthand :list default-node-analyzer))

(def map-node
  (make-shorthand :map default-node-analyzer))

(def set-node
  (make-shorthand :set default-node-analyzer))

(def vector-node
  (make-shorthand :vector default-node-analyzer))

(def string-node
  (make-shorthand :string default-node-analyzer))

(def number-node
  (make-shorthand :number default-node-analyzer))

(def boolean-node
  (make-shorthand :boolean default-node-analyzer))
