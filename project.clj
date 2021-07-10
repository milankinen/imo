(defproject imo "0.0.1"
  :description "Opinionated Clojure(Script) code formatting"
  :url "https://github.com/milankinen/imo"
  :license {:name "MIT" :url "https://opensource.org/licenses/MIT"}
  :plugins [[lein-ancient "0.6.15"]
            [lein-shell "0.5.0"]]
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/tools.cli "1.0.206"]
                 [com.cognitect/transit-clj "1.0.324"]
                 [io.github.java-diff-utils/java-diff-utils "4.10"]
                 [expound "0.8.9"]]
  :main ^:skip-aot imo.main
  :source-paths ["src/clj"]
  :java-source-paths ["src/java"]
  :target-path "target/%s"
  :uberjar-name "imo.jar"
  :global-vars {*warn-on-reflection* true}
  :profiles {:dev     {:jvm-opts     [~(str "-Dimo.version=" :project/version)]
                       :repl-options {:init-ns repl}
                       :global-vars  {*warn-on-reflection* true}}
             :test    {:dependencies [[eftest "0.5.9"]]}
             :clitest {:global-vars {*warn-on-reflection* false}}
             :uberjar {:global-vars   {*assert* false}
                       :clean-targets ["target/uberjar" "target/native"]
                       :jvm-opts      [~(str "-Dimo.version=" :project/version)]
                       :aot           :all}}
  :aliases {"i"            ["do"
                            ["with-profile" "+dev,+test" "deps"]
                            ["with-profile" "+dev,+test" "deps" ":tree"]]
            "test"         ["with-profile" "+dev,+test,+clitest" "trampoline" "run" "-m" "test-runner/run-from-cli!"]
            "t"            "test"
            "native-image" ["do"
                            ["shell" "./scripts/setup_graalvm.sh"]
                            ["shell" "./scripts/build_native_image.sh"]]
            "linux-image"  ["shell" "./scripts/build_linux_image_with_docker.sh"]
            "fmt"          ["with-profile" "+dev,+test" "trampoline" "run" "-m" "repl/format-project!"]})
