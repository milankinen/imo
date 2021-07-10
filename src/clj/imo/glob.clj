(ns imo.glob
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import (java.io File)
           (java.nio.file FileSystems Path)
           (imo ImoException)))

(defn- walk [^File dir wilcard-dirs? [glob & globs]]
  (if (some? glob)
    (if (= "**" glob)
      (let [full-glob (string/join "/" (cons glob globs))
            matcher (.getPathMatcher (FileSystems/getDefault) (str "glob:" full-glob))]
        (->> (file-seq dir)
             (filter #(and (.isFile ^File %)
                           (.matches matcher (Path/of (.toURI ^File %)))))))
      (let [matcher (.getPathMatcher (FileSystems/getDefault) (str "glob:**/" glob))
            file-list (seq (.listFiles ^File dir))]
        (concat
          (when (empty? globs)
            (->> (filter #(.isFile ^File %) file-list)
                 (filter #(.matches matcher (Path/of (.toURI ^File %))))))
          (->> (filter #(.isDirectory ^File %) file-list)
               (filter #(or wilcard-dirs? (.matches matcher (Path/of (.toURI ^File %)))))
               (mapcat #(walk % wilcard-dirs? globs))))))
    []))

(defn files
  "Returns a sequence of files matching the given glob"
  [glob]
  {:pre [(string? glob)]}
  (when (empty? glob)
    (throw (ImoException. "File pattern can't be empty")))
  (cond
    (string/starts-with? glob "/")
    (walk (io/file "/") false (string/split (subs glob 1) #"/"))
    (string/starts-with? glob "./")
    (walk (io/file ".") false (string/split (subs glob 2) #"/"))
    :else (walk (io/file ".") false (string/split glob #"/"))))
