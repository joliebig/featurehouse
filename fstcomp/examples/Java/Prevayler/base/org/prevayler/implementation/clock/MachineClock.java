package org.prevayler.implementation.clock;
import java.util.Date;

/** 
 * A Clock that uses the local machine clock (System.currentTimeMillis()) as its time source.
 */
public class MachineClock extends BrokenClock {
  /** 
 * @return The local machine time.
 */
  public synchronized Date time(){
    update();
    return super.time();
  }
  private synchronized void update(){
    long newTime=System.currentTimeMillis();
    if (newTime != _millis)     advanceTo(new Date(newTime));
  }
}
