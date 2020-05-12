package imo;

public class AnalysisException extends RuntimeException {
  public final int startLine;
  public final int startCol;
  public final int endLine;
  public final int endCol;

  public AnalysisException(String message, int startLine, int startCol, int endLine, int endCol) {
    super(message);
    this.startLine = startLine;
    this.startCol = startCol;
    this.endLine = endLine;
    this.endCol = endCol;
  }
}
