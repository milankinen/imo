package imo;

public class AnalysisException extends RuntimeException {
  public final Object causingNode;

  public AnalysisException(String msg, Object causingNode) {
    super(msg, null, false, false);
    this.causingNode = causingNode;
  }
}
