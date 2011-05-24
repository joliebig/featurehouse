package org.prevayler.foundation.network;
import java.io.IOException;

public interface ObjectReceiver {
  public void receive(  Object object) throws IOException ;
  public void close() throws IOException ;
}
