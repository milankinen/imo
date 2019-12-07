(ns imo.main
  (:require [clojure.tools.cli :as cli]
            [clojure.java.io :as io]
            [imo.core :as imo]
            [imo.logger :refer [vv warn] :as logger]
            [imo.config :as config]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [schema.core :as s])
  (:gen-class)
  (:import (java.io File ByteArrayInputStream)
           (imo UserCausedException)
           (java.security MessageDigest)))

; For tests
(defonce ^:dynamic *exit-jvm* true)

(defn user-ex [errors]
  (UserCausedException. (string/join "\n" errors)))

; CLI options and help

(def ^:private cli-options
  [["-h" "--help" "Show help"]
   ["-c" "--config FILE" "Path to configuration file"]
   [nil "--config-override EDN" "Overrides to the configuration in EDN string"]
   [nil "--cache-file FILE" "Cache file for already checked source files detection"]
   ["-v" nil "Increment verbosity level (-v or -vv or -vvv)"
    :id :verbosity
    :default 0
    :update-fn inc]])

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

; Config

(def ^:private default-config-file ".imo.edn")

(defn- parse-config [input]
  (try
    (binding [*read-eval* false]
      (-> (slurp (io/input-stream input))
          (edn/read-string)))
    (catch Exception ex
      (throw (user-ex ["Config parsing failed: " (.getMessage ex)])))))

(defn- config-merge [default user-config overrides]
  (letfn [(m [x y]
            (if (and (map? x) (map? y))
              (merge-with m x y)
              y))]
    (-> (m default user-config)
        (m overrides))))

(defn- load-config [opts]
  (let [user-config (if-let [c (get opts :config)]
                      (let [f (io/file c)]
                        (when-not (and (.exists ^File f) (.isFile ^File f))
                          (throw (user-ex [(str "Config file does not exist: " c)])))
                        (parse-config f))
                      (let [f (io/file default-config-file)]
                        (if (and (.exists ^File f) (.isFile ^File f))
                          (parse-config f)
                          {})))
        overrides (if-let [overrides (get opts :config-override)]
                    (parse-config (ByteArrayInputStream. (.getBytes ^String overrides)))
                    {})
        final-config (config-merge config/defaults user-config overrides)]
    (when-let [error-msg (config/check final-config)]
      (throw (user-ex [error-msg])))
    final-config))

; Caching

(def ^:private ^:const cache-file-version (str (or (System/getProperty "imo.version") "dev") "_1"))

(def ^:private CacheFile
  {:version s/Str
   :files   {s/Str s/Str}})

(defprotocol ICache
  (cached? [_ source-file ^String contents])
  (cache! [_ source-file ^String contents])
  (store! [_]))

(defn- md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn- relative-path [^File cache-file ^File source-file]
  (-> (.getParentFile ^File cache-file)
      (.toPath)
      (.relativize (.toPath ^File source-file))
      (.toString)))

(defrecord FileCache [cache-file lookup]
  ICache
  (cached? [_ source-file contents]
    (if (instance? File source-file)
      (let [checksum (md5 contents)
            path (relative-path cache-file source-file)]
        (= checksum (get @lookup path)))
      false))
  (cache! [_ source-file contents]
    (when (instance? File source-file)
      (let [checksum (md5 contents)
            path (relative-path cache-file source-file)]
        (swap! lookup assoc path checksum))))
  (store! [_]
    (try
      (vv "Writing cache file: " (.getPath ^File cache-file))
      (-> (.getParentFile ^File cache-file)
          (.mkdirs))
      (->> (pr-str {:version cache-file-version :files @lookup})
           (spit cache-file))
      (catch Exception ex
        (warn "Cache file writing failed: " (.getMessage ^Exception ex))))))

(def ^:private noop-cache
  (reify ICache
    (cached? [_ _ _] false)
    (cache! [_ _ _] nil)
    (store! [_] nil)))

(defn- load-cache [opts]
  (if-let [cache-file (get opts :cache-file)]
    (try
      (vv "Reading cache file: " cache-file)
      (let [file (io/file cache-file)
            contents (when (.exists ^File file)
                       (let [c (binding [*read-eval* false]
                                 (edn/read-string (slurp file)))]
                         (when (s/check CacheFile c)
                           (throw (user-ex ["Cache file is not valid"])))
                         c))]
        (if (= cache-file-version (:version contents))
          (->FileCache file (atom (:files contents)))
          (->FileCache file (atom {}))))
      (catch UserCausedException ex
        (throw ex))
      (catch Exception ex
        (throw (user-ex [(str "Cache file load failure: " (.getMessage ^Exception ex))]))))
    noop-cache))


; Input/output

(defn- parse-files [args]
  (if (= ["-"] args)
    [[] true]
    (if-let [errors (->> (map io/file args)
                         (keep #(cond
                                  (not (.exists ^File %)) (str "File does not exist: " %)
                                  (not (.isFile ^File %)) (str "Not a file: " %)
                                  :else nil))
                         (seq))]
      (throw (user-ex errors))
      [(map io/file args) false])))

(defn- format! [config inputs+outputs cache]
  (doseq [[in out] inputs+outputs]
    (let [name (when (instance? File in) (.getName ^File in))]
      (when name (vv "Formatting file: " name))
      (binding [logger/*current-file* name]
        (let [src-in (slurp in)]
          (if-not (cached? cache in src-in)
            (let [src-out (imo/format-source config src-in)]
              (spit out src-out)
              (cache! cache out src-out))
            (vv "File (" name ") found from cache, skipping...")))))))

; Entrypoint

(defn -main
  "Command line entry point for imo"
  [& args]
  (try
    (let [{:keys [options
                  arguments
                  summary
                  errors]} (cli/parse-opts args cli-options)]
      (cond
        (:help options)
        (print-help summary)

        (seq errors)
        (throw (user-ex errors))

        :else
        (let [[files stdin?] (parse-files arguments)
              cache (load-cache options)
              inputs+outputs (if-not stdin?
                               (map #(do [% %]) files)
                               [[*in* *out*]])
              format-config (load-config options)
              log-level (get options :verbosity 0)]
          (binding [logger/*debug-out* (if stdin? *err* *out*)
                    logger/*log-level* log-level]
            (format! format-config inputs+outputs cache)
            (store! cache))))
      (exit 0))
    (catch UserCausedException ex
      (binding [*out* *err*]
        (println (.getMessage ex)))
      (exit 1))
    (catch Exception ex
      (binding [*out* *err*]
        (println "Unexpected error occurred, please raise an issue at https://github.com/milankinen/imo/issues/new")
        (.printStackTrace ^Throwable ex))
      (exit 2))))

(comment

  (alter-var-root #'*exit-jvm* (constantly false))
  (-main "--help")

  (parse-opts ["-c" "foo.bar"] cli-options)

  '-)