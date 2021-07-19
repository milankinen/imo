package imo;

import java.util.Arrays;

public class Util {
  /**
   * Same as Math.max but only for longs, in order avoid
   * reflection in Clojure calls.
   */
  public static long maxLong(long a, long b) {
    return a >= b ? a : b;
  }

  public static boolean isSimpleNameStr(String s) {
    int i = s.indexOf('/');
    return i == -1 || "/".equals(s);
  }

  public static String spaces(int cols) {
    char[] chars = new char[cols];
    Arrays.fill(chars, ' ');
    return String.valueOf(chars);
  }
}
