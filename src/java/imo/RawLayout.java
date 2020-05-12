package imo;

import clojure.lang.*;

public class RawLayout extends Layout {
  public static final Keyword KIND = Keyword.intern("raw");

  public static Layout create(String s, boolean mightContainNewLines) {
    assert mightContainNewLines || !s.contains("\n");
    if (mightContainNewLines) {
      Util.StringStats stats = Util.stats(s);
      long bits = Layout.toBits(
          stats.fullLines > 0,
          stats.firstLineWidth,
          stats.lastLineWidth,
          stats.maxWidth,
          0,
          false,
          false,
          stats.fullLines > 0,
          0);
      return new RawLayout(bits, s, null);
    } else {
      int len = s.length();
      long bits = Layout.toBits(
          false,
          len,
          len,
          len,
          0,
          false,
          false,
          false,
          0);
      return new RawLayout(bits, s, null);
    }
  }

  private final String _content;

  private RawLayout(long bits, String content, IPersistentMap meta) {
    super(bits, meta);
    _content = content;
  }

  @Override
  public Keyword kind() {
    return KIND;
  }

  @Override
  public ISeq inspectChildren() {
    return PersistentVector.create(_content).seq();
  }

  @Override
  public Layout shrink(int offset, int alignment, int targetWidth, int targetPrecedence) {
    throw new IllegalStateException("Raw layout has no alternative layouts besides primary one");
  }

  @Override
  public void print(StringBuilder sb, int offset) {
    sb.append(_content);
  }

  @Override
  public IObj withMeta(IPersistentMap meta) {
    return new RawLayout(bits, _content, meta);
  }
}
