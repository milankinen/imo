(defproject imo "0.1.0-SNAPSHOT"
  :description "Opinionated Clojure(Script) code formatting"
  :url "https://github.com/milankinen/imo"
  :license {:name "MIT" :url "https://opensource.org/licenses/MIT"}
  :plugins [[lein-ancient "0.6.15"]]
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.match "0.3.0"]]
  :main ^:skip-aot imo.main
  :source-paths ["src/clj"]
  :java-source-paths ["src/java"]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
