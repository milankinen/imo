package imo;

import clojure.lang.*;

import java.util.Map;

public class SelectLayout extends Layout {
  public static final Keyword KIND = Keyword.intern("select");
  private static final Keyword SELECTOR_FN = Keyword.intern("selector-fn");
  private static final Keyword STAGED_CHILDREN = Keyword.intern("staged-children");

  public static Layout create(IFn selector,
                              Seqable children,
                              int width,
                              int precedence,
                              IPersistentMap meta) {
    IPersistentVector output = (IPersistentVector) selector.invoke(children, width, precedence);
    assert output != null && output.count() == 2;
    Layout selected = (Layout) output.nth(0);
    assert selected != null;
    children = (Seqable) output.nth(1);
    assert children != null;
    return new SelectLayout(selected, selector, children, meta);
  }

  private final IFn _selector;
  private final Layout _selected;
  private final Seqable _children;

  private SelectLayout(Layout selected, IFn selector, Seqable children, IPersistentMap meta) {
    super(selected.bits, meta);
    _selector = selector;
    _selected = selected;
    _children = children;
  }

  @Override
  public Keyword kind() {
    return KIND;
  }

  @Override
  public ISeq inspectChildren() {
    return PersistentVector.create(_selected).seq();
  }

  @Override
  public IPersistentMap inspectProps() {
    return PersistentArrayMap.create(Map.of(
        SELECTOR_FN, _selector,
        STAGED_CHILDREN, _children
    ));
  }

  @Override
  public Layout shrink(int offset, int alignment, int targetWidth, int targetPrecedence) {
    return create(_selector, _children, targetWidth - offset, targetPrecedence, _meta);
  }

  @Override
  public void print(StringBuilder sb, int offset) {
    _selected.print(sb, offset);
  }

  @Override
  public IObj withMeta(IPersistentMap meta) {
    return new SelectLayout(_selected, _selector, _children, meta);
  }
}
