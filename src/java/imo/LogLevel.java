package imo;

public class LogLevel {

  private static int logLevel = 0;

  public static void setLogLevel(int level) {
    logLevel = level;
  }

  public static boolean isLogLevelEnabled(int level) {
    return logLevel >= level;
  }
}
