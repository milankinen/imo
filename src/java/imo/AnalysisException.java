package imo;

public class AnalysisException extends RuntimeException {
  public final int line;
  public final int col;

  public AnalysisException(String message, int line, int col) {
    super(message);
    this.line = line;
    this.col = col;
  }
}
