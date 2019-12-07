(defproject imo "0.1.0-SNAPSHOT"
  :description "Opinionated Clojure(Script) code formatting"
  :url "https://github.com/milankinen/imo"
  :license {:name "MIT" :url "https://opensource.org/licenses/MIT"}
  :plugins [[lein-ancient "0.6.15"]
            [lein-shell "0.5.0"]
            [lein-project-version "0.1.0"]]
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.match "0.3.0"]
                 [org.clojure/tools.cli "0.4.2"]
                 [prismatic/schema "1.1.12"]]
  :main ^:skip-aot imo.main
  :source-paths ["src/clj"]
  :java-source-paths ["src/java"]
  :target-path "target/%s"
  :uberjar-name "imo.jar"
  :global-vars {*warn-on-reflection* true}
  :profiles {:dev     {:jvm-opts     [~(str "-Dimo.version=" :project/version)]
                       :repl-options {:init-ns repl}}
             :test    {:dependencies [[io.github.java-diff-utils/java-diff-utils "4.5"]
                                      [eftest "0.5.9"]]
                       :global-vars  {*warn-on-reflection* false}}
             :uberjar {:global-vars {*assert* false}
                       :jvm-opts    [~(str "-Dimo.version=" :project/version)]
                       :aot         :all}}
  :aliases {"i"            ["do"
                            ["with-profile" "+dev,+test" "deps"]
                            ["with-profile" "+dev,+test" "deps" ":tree"]]
            "test"         ["with-profile" "+dev,+test" "trampoline" "run" "-m" "imo.test-runner/run-from-cli!"]
            "t"            "test"
            "native-image" ["do"
                            ["shell" "./scripts/setup_graalvm.sh" ~(-> (System/getProperty "java.version")
                                                                     (.replaceFirst "^1\\." "")
                                                                     (.split "\\.")
                                                                     (first))]
                            ["clean"]
                            ["uberjar"]
                            ["shell" "./scripts/build_native_image.sh"]]
            "linux-image"  ["shell" "./scripts/build_linux_image_with_docker.sh"]})
