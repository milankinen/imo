package imo;

import clojure.lang.ISeq;
import clojure.lang.ITransientCollection;

public class AnalysisState {
  public ITransientCollection analyzed;
  public ISeq remaining;
  public Object ctx;

  public AnalysisState(ITransientCollection analyzed, ISeq remaining, Object ctx) {
    this.analyzed = analyzed;
    this.remaining = remaining;
    this.ctx = ctx;
  }
}
