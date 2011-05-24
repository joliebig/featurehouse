package org.prevayler.foundation.monitor;
import java.io.File;

/** 
 * A Monitor for interesting events in the system.
 */
public interface Monitor {
  /** 
 * Something interesting happened.
 */
  void notify(  Class clazz,  String message);
  /** 
 * An interesting exception was thrown.
 */
  void notify(  Class clazz,  String message,  Exception ex);
  /** 
 * Something interesting happened regarding access to a file.
 */
  void notify(  Class clazz,  String message,  File file);
  /** 
 * An exception was thrown while trying to access a file.
 */
  void notify(  Class clazz,  String message,  File file,  Exception ex);
}
