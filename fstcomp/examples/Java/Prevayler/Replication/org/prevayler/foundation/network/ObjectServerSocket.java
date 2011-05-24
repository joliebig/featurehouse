package org.prevayler.foundation.network;
import java.io.IOException;

public interface ObjectServerSocket {
  ObjectSocket accept() throws IOException ;
  void close() throws IOException ;
}
