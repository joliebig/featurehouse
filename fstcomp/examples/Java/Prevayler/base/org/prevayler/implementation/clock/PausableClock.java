package org.prevayler.implementation.clock;
import java.util.Date;
import org.prevayler.Clock;

public class PausableClock implements Clock {
  private final Clock _realClock;
  private final BrokenClock _brokenClock=new BrokenClock();
  private Clock _activeClock;
  public PausableClock(  Clock realClock){
    _realClock=realClock;
    resume();
  }
  public synchronized Date time(){
    return _activeClock.time();
  }
  public synchronized void pause(){
    advanceTo(_realClock.time());
    _activeClock=_brokenClock;
  }
  public void advanceTo(  Date time){
    _brokenClock.advanceTo(time);
  }
  public synchronized void resume(){
    _activeClock=_realClock;
  }
  public Date realTime(){
    return _realClock.time();
  }
}
