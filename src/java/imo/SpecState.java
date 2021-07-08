package imo;

import clojure.lang.ISeq;
import clojure.lang.ITransientCollection;

public class SpecState {
  public ITransientCollection analyzed;
  public ISeq remaining;
  public Object ctx;

  public SpecState(ITransientCollection analyzed, ISeq remaining, Object ctx) {
    this.analyzed = analyzed;
    this.remaining = remaining;
    this.ctx = ctx;
  }
}
