package org.prevayler.foundation.monitor;
import java.io.File;

/** 
 * A Null Monitor, that does no logging at all.
 */
public class NullMonitor implements Monitor {
  /** 
 * Does nothing.
 */
  public void notify(  Class clazz,  String message,  File file,  Exception exception){
  }
  /** 
 * Does nothing.
 */
  public void notify(  Class clazz,  String message){
  }
  /** 
 * Does nothing.
 */
  public void notify(  Class clazz,  String message,  Exception ex){
  }
  /** 
 * Does nothing.
 */
  public void notify(  Class clazz,  String message,  File file){
  }
}
