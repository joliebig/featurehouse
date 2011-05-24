package org.prevayler;
import org.prevayler.foundation.monitor.Monitor;
import org.prevayler.foundation.monitor.SimpleMonitor;
public class PrevaylerFactory {
  private Monitor _monitor;
  /** 
 * Assigns a monitor object to receive notifications from Prevayler. This is
 * useful for logging or sending eMails to system administrators, for
 * example. If this method is not called or if null is passed as a
 * parameter, a SimpleMonitor will be used to log notification on
 * System.err.
 * @param monitorthe Monitor implementation to use.
 * @see org.prevayler.foundation.monitor.SimpleMonitor
 */
  public void configureMonitor(  Monitor monitor){
    _monitor=monitor;
  }
  private Monitor monitor(){
    return _monitor != null ? _monitor : new SimpleMonitor(System.err);
  }
}
