package imo;

import clojure.lang.*;

public abstract class Layout implements IObj {
  public static final long MULTILINE_BIT = 1L << 63;
  public static final long FIRST_LINE_SHRINKABLE_BIT = 1L << 62;
  public static final long LAST_LINE_SHRINKABLE_BIT = 1L << 61;
  public static final long LAST_LINE_ABSOLUTE_OFFSET_BIT = 1L << 60;
  public static final long WIDTH_MASK = 0xFFF;
  public static final long PRECEDENCE_MASK = 0xFF;

  protected final IPersistentMap _meta;
  public final long bits;

  protected Layout(long bits, IPersistentMap meta) {
    this.bits = bits;
    this._meta = meta;
  }

  public abstract Keyword kind();

  public abstract ISeq inspectChildren();

  public IPersistentMap inspectProps() {
    return null;
  }

  public abstract Layout shrink(int offset, int alignment, int targetWidth, int targetPrecedence);

  public abstract void print(StringBuilder sb, int offset);

  @Override
  public IPersistentMap meta() {
    return _meta;
  }

  //
  //

  public static Layout shrinkUntil(Layout layout, int offset, int alignment, int targetWidth, int targetPrecedence) {
    while (true) {
      long bits = layout.bits;
      if (shrinkPrecedence(bits) < targetPrecedence || offset + maxShrinkableWidth(bits) <= targetWidth) {
        break;
      } else {
        layout = layout.shrink(offset, alignment, targetWidth, targetPrecedence);
      }
    }
    return layout;
  }

  public static long toBits(boolean multiline,
                            int firstLineWidth,
                            int lastLineWidth,
                            int maxWidth,
                            int maxShrinkableWidth,
                            boolean firstLineShrinkable,
                            boolean lastLineShrinkable,
                            boolean lastLineAbsoluteOffset,
                            int shrinkPrecedence) {
    return (multiline ? MULTILINE_BIT : 0)
        | (firstLineShrinkable ? FIRST_LINE_SHRINKABLE_BIT : 0)
        | (lastLineShrinkable ? LAST_LINE_SHRINKABLE_BIT : 0)
        | (lastLineAbsoluteOffset ? LAST_LINE_ABSOLUTE_OFFSET_BIT : 0)
        | ((Math.min(firstLineWidth, WIDTH_MASK) & WIDTH_MASK) << 44)
        | ((Math.min(lastLineWidth, WIDTH_MASK) & WIDTH_MASK) << 32)
        | ((Math.min(maxShrinkableWidth, WIDTH_MASK) & WIDTH_MASK) << 20)
        | ((Math.min(maxWidth, WIDTH_MASK) & WIDTH_MASK) << 8)
        | (shrinkPrecedence & PRECEDENCE_MASK);
  }

  public static boolean isMultiline(long bits) {
    return (bits & MULTILINE_BIT) != 0;
  }

  public static boolean isFirstLineShrinkable(long bits) {
    return (bits & FIRST_LINE_SHRINKABLE_BIT) != 0;
  }

  public static boolean isLastLineShrinkable(long bits) {
    return (bits & LAST_LINE_SHRINKABLE_BIT) != 0;
  }

  public static boolean isLastLineOffsetAbsolute(long bits) {
    return (bits & LAST_LINE_ABSOLUTE_OFFSET_BIT) != 0;
  }

  public static int firstLineWidth(long bits) {
    return (int) ((bits >> 44) & WIDTH_MASK);
  }

  public static int lastLineWidth(long bits) {
    return (int) ((bits >> 32) & WIDTH_MASK);
  }

  public static int maxShrinkableWidth(long bits) {
    return (int) ((bits >> 20) & WIDTH_MASK);
  }

  public static int maxWidth(long bits) {
    return (int) ((bits >> 8) & WIDTH_MASK);
  }

  public static int shrinkPrecedence(long bits) {
    return (int) (bits & PRECEDENCE_MASK);
  }

  public static int nextOffset(int offset, Layout layout) {
    long bits = layout.bits;
    if (isLastLineOffsetAbsolute(bits)) {
      return lastLineWidth(bits);
    } else {
      return offset + lastLineWidth(bits);
    }
  }
}
