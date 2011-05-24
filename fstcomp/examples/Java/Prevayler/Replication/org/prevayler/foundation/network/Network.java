package org.prevayler.foundation.network;
import java.io.IOException;

public interface Network {
  void startService(  Service service,  int port) throws IOException ;
  void stopService(  int port) throws IOException ;
  ObjectReceiver findServer(  String ipAddress,  int port,  ObjectReceiver client) throws IOException ;
}
