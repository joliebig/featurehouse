package org.prevayler.foundation.network;
import java.io.IOException;
import java.net.ServerSocket;

public class ObjectServerSocketImpl implements ObjectServerSocket {
  private final ServerSocket _serverSocket;
  public ObjectServerSocketImpl(  int port) throws IOException {
    _serverSocket=new ServerSocket(port);
  }
  public ObjectSocket accept() throws IOException {
    return new ObjectSocketImpl(_serverSocket.accept());
  }
  public void close() throws IOException {
    _serverSocket.close();
  }
}
