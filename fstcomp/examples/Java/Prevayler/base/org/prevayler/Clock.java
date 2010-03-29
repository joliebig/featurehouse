package org.prevayler;

/** 
 * Tells the time.
 * @see Prevayler
 */
public interface Clock {
  /** 
 * Tells the time.
 * @return A Date greater or equal to the one returned by the last call to this method. If the time is the same as the last call, the SAME Date object is returned rather than a new, equal one.
 */
  public java.util.Date time();
}
