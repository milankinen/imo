package imo;

import clojure.lang.*;

import static imo.Util.concat;

public class InlineLayout extends Layout {
  public static final Keyword KIND = Keyword.intern("inline");

  public static Layout create(ISeq children, IPersistentMap meta) {
    if (children == null) {
      return null;
    }

    ITransientCollection output = PersistentVector.EMPTY.asTransient();
    boolean multiline = false;
    boolean firstLineShrinkable = false;
    boolean lastLineShrinkable = false;
    boolean lastLineOffsetAbsolute = false;
    int firstLineWidth = 0;
    int lastLineWidth = 0;
    int maxWidth = 0;
    int maxShrinkableWidth = 0;
    int shrinkPrecedence = 0;
    int shrinkOffset = -1;
    int shrinkIdx = -1;

    while (children != null) {
      Object child = children.first();
      if (child instanceof Layout) {
        output = output.conj(child);

        long childBits = ((Layout) child).bits;

        int childShrinkPrecedence = shrinkPrecedence(childBits);
        if (childShrinkPrecedence > shrinkPrecedence) {
          shrinkPrecedence = childShrinkPrecedence;
          shrinkIdx = ((Counted) output).count() - 1;
          shrinkOffset = lastLineOffsetAbsolute ? -lastLineWidth : lastLineWidth;
        }

        if (isMultiline(childBits)) {
          if (!multiline) {
            multiline = true;
            firstLineWidth = lastLineWidth + firstLineWidth(childBits);
            firstLineShrinkable = lastLineShrinkable || isFirstLineShrinkable(childBits);
            if (firstLineShrinkable) {
              maxShrinkableWidth = Math.max(maxShrinkableWidth, firstLineWidth);
            }
          }
          lastLineWidth = lastLineWidth(childBits);
          lastLineShrinkable = isLastLineShrinkable(childBits);
          lastLineOffsetAbsolute = isLastLineOffsetAbsolute(childBits);
          maxShrinkableWidth = Math.max(maxShrinkableWidth, maxShrinkableWidth(childBits));
          maxWidth = Math.max(maxWidth, maxWidth(childBits));
        } else {
          lastLineWidth += firstLineWidth(childBits);
          lastLineShrinkable = lastLineShrinkable || isLastLineShrinkable(childBits);
        }

        children = children.next();
      } else if (child instanceof Seqable) {
        children = concat(((Seqable) child).seq(), children.next());
      } else if (child == null) {
        children = children.next();
      } else {
        throw new IllegalArgumentException("Invalid inline child: " + child);
      }
    }

    PersistentVector layoutChildren = (PersistentVector) output.persistent();
    switch (layoutChildren.count()) {
      case 0:
        return null;
      case 1:
        return (Layout) layoutChildren.nth(0);
      default:
        maxShrinkableWidth = lastLineShrinkable
            ? Math.max(lastLineWidth, maxShrinkableWidth)
            : maxShrinkableWidth;

        long bits = Layout.toBits(
            multiline,
            multiline ? firstLineWidth : lastLineWidth,
            lastLineWidth,
            Math.max(maxWidth, lastLineWidth),
            maxShrinkableWidth,
            multiline ? firstLineShrinkable : lastLineShrinkable,
            lastLineShrinkable,
            lastLineOffsetAbsolute,
            shrinkPrecedence);

        return new InlineLayout(bits, layoutChildren, shrinkIdx, shrinkOffset, meta);
    }
  }


  private final PersistentVector _children;
  private final int _shrinkIdx;
  private final int _shrinkOffset;

  private InlineLayout(long bits, PersistentVector children, int shrinkIdx, int shrinkOffset, IPersistentMap meta) {
    super(bits, meta);
    this._children = children;
    this._shrinkIdx = shrinkIdx;
    this._shrinkOffset = shrinkOffset;
  }

  @Override
  public IPersistentVector inspect() {
    return _children.cons(KIND);
  }

  @Override
  public Layout shrink(int offset, int alignment, int targetWidth, int targetPreference) {
    assert _shrinkIdx >= 0;
    Layout childToShrink = (Layout) _children.get(_shrinkIdx);
    offset = _shrinkOffset < 0 ? _shrinkOffset : offset + _shrinkOffset;
    PersistentVector nextChildren = _children.assocN(
        _shrinkIdx,
        childToShrink.shrink(offset, alignment, targetWidth, targetPreference));
    return create(nextChildren.seq(), _meta);
  }

  @Override
  public void print(StringBuilder sb, int offset) {
    for (Object child : _children) {
      ((Layout) child).print(sb, offset);
      offset = nextOffset(offset, (Layout) child);
    }
  }

  @Override
  public IObj withMeta(IPersistentMap meta) {
    return new InlineLayout(bits, _children, _shrinkIdx, _shrinkOffset, meta);
  }
}
