package imo;

import clojure.lang.*;

import static imo.Util.concat;

public class AlignedLayout extends Layout {
  public static final Keyword KIND = Keyword.intern("aligned");

  public static Layout create(ISeq children, IPersistentMap meta) {
    if (children == null) {
      return null;
    }
    ITransientCollection output = PersistentVector.EMPTY.asTransient();
    boolean firstLineShrinkable = false;
    boolean lastLineShrinkable = false;
    boolean lastLineOffsetAbsolute = false;
    int lines = 0;
    int firstLineWidth = 0;
    int lastLineWidth = 0;
    int maxWidth = 0;
    int maxShrinkableWidth = 0;
    int shrinkPrecedence = 0;

    while (children != null) {
      Object child = children.first();
      if (child instanceof Layout) {
        long childBits = ((Layout) child).bits;
        if (lines == 0) {
          firstLineWidth = firstLineWidth(childBits);
          firstLineShrinkable = isFirstLineShrinkable(childBits);
          maxShrinkableWidth = maxShrinkableWidth(childBits);
          maxWidth = maxWidth(childBits);
        } else {
          maxShrinkableWidth = Math.max(maxShrinkableWidth, maxShrinkableWidth(childBits));
          maxWidth = Math.max(maxWidth, maxWidth(childBits));
        }
        shrinkPrecedence = Math.max(shrinkPrecedence, shrinkPrecedence(childBits));
        lastLineShrinkable = isLastLineShrinkable(childBits);
        lastLineOffsetAbsolute = isLastLineOffsetAbsolute(childBits);
        lastLineWidth = lastLineWidth(childBits);
        output = output.conj(child);
        lines++;
        children = children.next();
      } else if (child instanceof Seqable) {
        children = concat(((Seqable) child).seq(), children.next());
      } else if (child == null) {
        children = children.next();
      } else {
        throw new IllegalArgumentException("Invalid aligned child: " + child);
      }
    }

    PersistentVector layoutChildren = (PersistentVector) output.persistent();
    switch (layoutChildren.count()) {
      case 0:
        return null;
      case 1:
        return (Layout) layoutChildren.nth(0);
      default:
        long bits = Layout.toBits(
            lines > 0,
            firstLineWidth,
            lastLineWidth,
            maxWidth,
            maxShrinkableWidth,
            firstLineShrinkable,
            lastLineShrinkable,
            lastLineOffsetAbsolute,
            shrinkPrecedence);
        return new AlignedLayout(bits, layoutChildren, meta);
    }
  }

  private final PersistentVector _children;

  private AlignedLayout(long bits, PersistentVector children, IPersistentMap meta) {
    super(bits, meta);
    _children = children;
  }

  @Override
  public Keyword kind() {
    return KIND;
  }

  @Override
  public ISeq inspectChildren() {
    return _children.seq();
  }

  @Override
  public Layout shrink(int offset, int alignment, int targetWidth, int targetPreference) {
    ITransientCollection nextChildren = PersistentVector.EMPTY.asTransient();
    for (int i = 0, n = _children.count(); i < n; i++) {
      Layout child = (Layout) _children.nth(i);
      child = shrinkUntil(child, offset, offset, targetWidth, targetPreference);
      nextChildren = nextChildren.conj(child);
    }
    return create(nextChildren.persistent().seq(), _meta);
  }

  @Override
  public void print(StringBuilder sb, int offset) {
    String alignment = Util.spaces(offset);
    ((Layout) _children.nth(0)).print(sb, offset);
    for (int i = 1, n = _children.count(); i < n; i++) {
      sb.append('\n');
      sb.append(alignment);
      ((Layout) _children.nth(i)).print(sb, offset);
    }
  }

  @Override
  public IObj withMeta(IPersistentMap meta) {
    return new AlignedLayout(bits, _children, meta);
  }
}
