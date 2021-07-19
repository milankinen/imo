package imo;

import clojure.java.api.Clojure;
import clojure.lang.IFn;

import java.util.Arrays;
import java.util.stream.Collectors;

public class PerfTester {
  public static String source = "";
  public static void main(String[] args) {
    if (args.length == 0) {
      System.err.println("Add formatted code as an argument");
      System.exit(1);
    }
    source = String.join(" ", args);
    System.out.println("Format source code:\n" + source + "\n");
    IFn readString = Clojure.var("clojure.core", "read-string");
    IFn eval = Clojure.var("clojure.core", "eval");
    eval.invoke(readString.invoke("(binding [*assert* false] (load-file \"src/clj/imo/core.clj\"))"));
    eval.invoke(readString.invoke("(binding [*assert* false] (load-file \"src/clj/imo/config.clj\"))"));
    Object code = readString.invoke("" +
        "(binding [*assert* false]" +
        " (require '[imo.config])" +
        " (require '[imo.core])" +
        " (let [src imo.PerfTester/source]" +
        "   (dotimes [_ 1]" +
        "     (time (->> (imo.core/read src)" +
        "                (imo.core/analyze imo.config/defaults)" +
        "                (imo.core/format imo.config/defaults))))))");
    eval.invoke(code);
  }
}