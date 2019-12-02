(ns imo.main
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :as io]
            [imo.core :as imo])
  (:gen-class))

; For tests
(def ^:dynamic *exit-jvm* true)

(def ^:private cli-options
  [["-h" "--help"]])

(defn- exit [code]
  (if *exit-jvm*
    (System/exit code)
    code))

(defn- print-help [opts-summary]
  (println "Usage: imo [options ...] files...     format specified files in-place")
  (println "   or: imo [options ...] -            read contents from stdin and print formatted content to stdout")
  (println "")
  (println "Options:")
  (println opts-summary))

(defn- parse-files [args]
  (if (= ["-"] args)
    [[] [] true]
    (if-let [errors (->> (map io/file args)
                         (keep #(cond
                                  (not (.exists %)) (str "File does not exist: " %)
                                  (not (.isFile %)) (str "Not a file: " %)
                                  :else nil))
                         (seq))]
      [[] errors false]
      [(map io/file args) [] false])))

(defn- print-errors [errors]
  (binding [*out* *err*]
    (doseq [e errors]
      (println e))
    (exit 1)))

(defn- merge-options [opts]
  (merge {:style :preserve} opts))

(defn- format! [options inputs+outputs]
  (doseq [[in out] inputs+outputs]
    (let [src-in (slurp in)
          src-out (imo/format options src-in)]
      (spit out src-out)))
  nil)


(defn -main
  "Command line entrypoint for imo"
  [& args]
  (let [{:keys [options
                arguments
                summary
                errors]} (parse-opts args cli-options)
        [files file-errors stdin?] (parse-files arguments)
        cli-errors (concat errors file-errors)]
    (cond
      (:help options)
      (do (print-help summary)
          (exit 0))

      (seq cli-errors)
      (do (print-errors cli-errors)
          (exit 1))

      :else
      (let [inputs+outputs (if-not stdin?
                             (map #(do [% %]) files)
                             [[*in* *out*]])
            format-opts (merge-options options)]
        (if-let [errors (seq (format! format-opts inputs+outputs))]
          (do (print-errors errors)
              (exit 1))
          (exit 0))))))
