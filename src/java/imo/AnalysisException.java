package imo;

import clojure.lang.IFn;

public class AnalysisException extends RuntimeException {
  private final IFn _getPosition;

  public AnalysisException(IFn getPosition, String msg) {
    super(msg, null, false, false);
    this._getPosition = getPosition;
  }

  public Object getPosition() {
    return _getPosition.invoke();
  }
}
