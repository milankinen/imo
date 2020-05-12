package imo;

import clojure.lang.*;

import java.util.Map;

public class AltLayout extends Layout {
  public static final Keyword KIND = Keyword.intern("alt");
  private static final Keyword SECONDARY_FN = Keyword.intern("secondary-layout-fn");
  private static final Keyword SECONDARY_PRECEDENCE = Keyword.intern("secondary-layout-precedence");

  public static Layout create(Layout primary,
                              IFn secondary,
                              int secondaryPrecedence,
                              IPersistentMap meta) {
    long primaryBits = primary.bits;
    long bits = Layout.toBits(
        isMultiline(primaryBits),
        firstLineWidth(primaryBits),
        lastLineWidth(primaryBits),
        maxWidth(primaryBits),
        maxWidth(primaryBits),
        true,
        true,
        isLastLineOffsetAbsolute(primaryBits),
        Math.max(shrinkPrecedence(primaryBits), secondaryPrecedence)
    );
    return new AltLayout(bits, primary, secondary, secondaryPrecedence, meta);
  }

  private final Layout _primary;
  private final IFn _secondary;
  private final int _secondaryPrecedence;

  private AltLayout(long bits, Layout primary, IFn secondary, int secondaryPrecedence, IPersistentMap meta) {
    super(bits, meta);
    _primary = primary;
    _secondary = secondary;
    _secondaryPrecedence = secondaryPrecedence;
  }

  @Override
  public Keyword kind() {
    return KIND;
  }

  @Override
  public ISeq inspectChildren() {
    return PersistentVector.create(_primary).seq();
  }

  @Override
  public IPersistentMap inspectProps() {
    return PersistentArrayMap.create(Map.of(
        SECONDARY_FN, _secondary,
        SECONDARY_PRECEDENCE, _secondaryPrecedence
    ));
  }

  @Override
  public Layout shrink(int offset, int alignment, int targetWidth, int targetPrecedence) {
    if (shrinkPrecedence(_primary.bits) > _secondaryPrecedence) {
      return create(
          _primary.shrink(offset, alignment, targetWidth, targetPrecedence),
          _secondary,
          _secondaryPrecedence,
          _meta);
    } else {
      return (Layout) _secondary.invoke();
    }
  }

  @Override
  public void print(StringBuilder sb, int offset) {
    _primary.print(sb, offset);
  }

  @Override
  public IObj withMeta(IPersistentMap meta) {
    return new AltLayout(bits, _primary, _secondary, _secondaryPrecedence, meta);
  }
}
