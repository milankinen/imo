package imo;

import clojure.java.api.Clojure;
import clojure.lang.IFn;


public class PerfTest {
  public static void main(String[] args) {
    IFn readString = Clojure.var("clojure.core", "read-string");
    IFn eval = Clojure.var("clojure.core", "eval");
    eval.invoke(readString.invoke("(binding [*assert* false] (load-file \"src/clj/imo/core.clj\"))"));
    eval.invoke(readString.invoke("(binding [*assert* false] (load-file \"src/clj/imo/config.clj\"))"));
    Object code = readString.invoke("" +
        "(binding [*assert* false imo.logger/*log-level* 5]" +
        " (require '[imo.config])" +
        " (require '[imo.core])" +
        " (let [clj-core (slurp \"test/__files__/clojure_core.clj\")]" +
        "   (dotimes [_ 5]" +
        "     (imo.core/format-source imo.config/defaults clj-core))))");
    eval.invoke(code);
  }
}
