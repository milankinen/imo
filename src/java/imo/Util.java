package imo;

import clojure.lang.ASeq;
import clojure.lang.IPersistentMap;
import clojure.lang.ISeq;
import clojure.lang.Obj;

import java.util.Arrays;

public class Util {
  /**
   * Same as `clojure.core/concat` but ignores laziness
   * and chunks etc.. for better performance.
   *
   * @param head Head part of the concatenated sequence
   * @param tail Tail part of the concatenated sequence
   * @return Concatenated sequence having both head and tail
   */
  public static ISeq concat(ISeq head, ISeq tail) {
    if (head == null) {
      return tail;
    } else if (tail == null) {
      return head;
    } else {
      return new FastCat(head, tail, null);
    }
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

  public static StringStats stats(String s) {
    int fullLines = 0;
    int firstLineWidth = 0;
    int maxWidth = 0;
    int lastLineWidth = 0;
    for (int i = 0, n = s.length(); i < n; i++) {
      if (s.charAt(i) == '\n') {
        if (fullLines == 0) {
          firstLineWidth = lastLineWidth;
          maxWidth = lastLineWidth;
        } else {
          maxWidth = Math.max(lastLineWidth, maxWidth);
        }
        ++fullLines;
        lastLineWidth = 0;
      } else {
        ++lastLineWidth;
      }
    }
    return new StringStats(
        fullLines == 0 ? lastLineWidth : firstLineWidth,
        fullLines == 0 ? lastLineWidth : Math.max(maxWidth, lastLineWidth),
        lastLineWidth,
        fullLines);
  }

  public static class StringStats {
    public final int firstLineWidth;
    public final int maxWidth;
    public final int lastLineWidth;
    public final int fullLines;

    public StringStats(int firstLineWidth, int maxWidth, int lastLineWidth, int fullLines) {
      this.firstLineWidth = firstLineWidth;
      this.maxWidth = maxWidth;
      this.lastLineWidth = lastLineWidth;
      this.fullLines = fullLines;
    }
  }

  private static class FastCat extends ASeq {
    private final ISeq _head;
    private final ISeq _tail;
    private final IPersistentMap _meta;

    private FastCat(ISeq head, ISeq tail, IPersistentMap meta) {
      _head = head;
      _tail = tail;
      _meta = meta;
    }

    @Override
    public Object first() {
      return _head.first();
    }

    @Override
    public ISeq next() {
      ISeq next = _head.next();
      if (next != null) {
        return new FastCat(next, _tail, _meta);
      } else {
        return _tail;
      }
    }

    @Override
    public Obj withMeta(IPersistentMap meta) {
      return new FastCat(_head, _tail, meta);
    }
  }
}
