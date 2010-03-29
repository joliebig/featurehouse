package org.prevayler.foundation;
import org.prevayler.foundation.monitor.Monitor;
public class DurableInputStream {
  private Monitor _monitor;
  DurableInputStream(  File file,  Monitor monitor) throws IOException {
    this(file);
    _monitor=monitor;
  }
  protected void hook75(  Exception ex,  String message){
    _monitor.notify(this.getClass(),message,_file,ex);
    original(ex,message);
  }
}
