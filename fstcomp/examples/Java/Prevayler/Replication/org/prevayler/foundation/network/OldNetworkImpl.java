package org.prevayler.foundation.network;
import java.io.IOException;

public class OldNetworkImpl implements OldNetwork {
  public ObjectSocket openSocket(  String serverIpAddress,  int serverPort) throws IOException {
    return new ObjectSocketImpl(serverIpAddress,serverPort);
  }
  public ObjectServerSocket openObjectServerSocket(  int port) throws IOException {
    return new ObjectServerSocketImpl(port);
  }
}
