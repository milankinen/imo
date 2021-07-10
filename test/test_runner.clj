(ns test-runner
  (:require [clojure.string :as string]
            [eftest.runner :as runner]
            [eftest.report.progress]))

(defn run-from-cli!
  "Runs all tests matching the given test regexps.
   Called from 'lein t', see project.clj
  "
  [& test-regexps]
  (let [matchers (map (fn [t]
                        (try
                          (let [re (re-pattern t)]
                            #(or (some? (re-matches re %))
                                 (string/includes? % t)))
                          (catch Exception _
                            #(string/includes? % t))))
                      test-regexps)
        accept? (if (seq matchers)
                  (fn [test-var]
                    (let [m (meta test-var)
                          test-name (str (ns-name (:ns m)) "/" (:name m))]
                      (first (filter #(% test-name) matchers))))
                  (constantly true))
        tests-to-run (->> (runner/find-tests "test/imo")
                          (filter accept?))
        options {:report eftest.report.progress/report}]
    (when (seq tests-to-run)
      (println "Running" (count tests-to-run) "tests..."))
    (let [res (runner/run-tests tests-to-run options)]
      (System/exit (+ (:fail res) (:error res))))))
