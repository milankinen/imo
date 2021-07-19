package imo;

import clojure.lang.Cons;
import clojure.lang.ISeq;
import clojure.lang.ITransientCollection;
import clojure.lang.PersistentVector;

public class LayoutBuilder {
  public final long targetWidth;
  public final long alternative;
  public ITransientCollection contents;
  public ISeq aligns;
  public long offset;
  public long relativeOffset;
  public long relativeWidth;
  public long lineBreaks;

  public LayoutBuilder(long startOffset, long targetWidth, long alternative) {
    this.offset = startOffset;
    this.targetWidth = targetWidth;
    this.alternative = alternative;
    this.contents = PersistentVector.EMPTY.asTransient();
    this.aligns = new Cons(startOffset, null);
    this.relativeOffset = 0;
    this.relativeWidth = 0;
  }

  public void setOffset(long offset) {
    this.offset = offset;
  }

  public void updateRelativeValues(long relativeOffset, long relativeWidth) {
    this.relativeOffset = relativeOffset >= 0 ? this.relativeOffset + relativeOffset : -1;
    this.relativeWidth = relativeWidth >= 0 ? Math.max(relativeWidth, this.relativeWidth) : -1;
  }

  public void addItem(Object item) {
    this.contents = this.contents.conj(item);
  }

  public void addLineBreaks(long breaks) {
    this.lineBreaks += breaks;
  }

  public void align(long offset) {
    this.aligns = new Cons(offset, this.aligns);
  }

  public void dealign() {
    this.aligns = this.aligns.next();
  }
}
