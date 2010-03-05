package org.prevayler.foundation;

public class StopWatch {
  private final long t0=System.currentTimeMillis();
  static public StopWatch start(){
    return new StopWatch();
  }
  public long millisEllapsed(){
    return System.currentTimeMillis() - t0;
  }
  public double secondsEllapsed(){
    return millisEllapsed() / 1000.0;
  }
  private StopWatch(){
  }
}
