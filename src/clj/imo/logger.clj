(ns imo.logger)

(def ^{:dynamic true :doc "Current log level for logging operations"}
  *log-level* 0)

(def ^{:dynamic true :doc "Current file to show in warning and debug logs"}
  *current-file* nil)

(def ^{:dynamic true :doc "Output channel for debug logs"}
  *debug-out* *out*)

(defn- print* [& xs]
  (doseq [x xs]
    (print x)))

(defn log [position prefix xs]
  (print* prefix)
  (when *current-file*
    (print* " " *current-file*))
  (when position
    (print* ":" (:line position))
    (when-let [col (:col position)]
      (print* ":" col "")))
  (print* ": ")
  (doseq [x xs]
    (print* x))
  (print* "\n")
  (flush))

;;
;; Public stuff
;;

(defn warn
  "Prints a warning for the given AST node."
  [position & xs]
  (binding [*out* *err*]
    (log position "WARN" xs)))

(defmacro v
  "Prints debug logging with level 1 (-v)"
  [& xs]
  `(when (>= *log-level* 1)
     (binding [*out* *debug-out*]
       (log nil "DEBUG" ~(vec xs)))))

(defmacro vv
  "Prints debug logging with level 2 (-vv)"
  [& xs]
  `(when (>= *log-level* 2)
     (binding [*out* *debug-out*]
       (log nil "DEBUG" ~(vec xs)))))

(defmacro vvv
  "Prints debug logging with level 3 (-vvv)"
  [& xs]
  `(when (>= *log-level* 3)
     (binding [*out* *debug-out*]
       (log nil "DEBUG" ~(vec xs)))))

(defmacro timed
  "Prints time"
  [operation & body]
  `(if (>= *log-level* 3)
     (let [op# ~operation]
       (vvv "start " op# "...")
       (let [start# (System/nanoTime)
             result# (do ~@body)
             end# (System/nanoTime)]
         (vvv (format "%s complete, took: %.2f ms" op# (/ (- end# start#) 1000000.0)))
         result#))
     (do ~@body)))
