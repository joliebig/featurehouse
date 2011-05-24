package org.prevayler.foundation.network;
import java.io.IOException;
import java.net.SocketException;

/** 
 * Provides a server connection service.
 * Uses a thread to wait for connections. It then creates a new instance of
 * a Receiver. 
 */
public class NetworkServerObjectReceiverImpl extends Thread implements NetworkServerObjectReceiver {
  private Service _service;
  private ObjectServerSocket _provider;
  private boolean _wantedOpen;
  private NetworkReceiverFactory _factory;
  public NetworkServerObjectReceiverImpl(  NetworkReceiverFactory factory,  Service service,  int port) throws IOException {
    this(factory,service,new ObjectServerSocketImpl(port));
  }
  protected NetworkServerObjectReceiverImpl(  NetworkReceiverFactory factory,  Service service,  ObjectServerSocket server){
    _factory=factory;
    _service=service;
    _provider=server;
    _wantedOpen=true;
    setName("Prevayler Network Server Receiver");
    setDaemon(true);
    start();
  }
  public void run(){
    while (_wantedOpen) {
      try {
        _factory.newReceiver(_service,_provider.accept());
      }
 catch (      SocketException sox) {
        _wantedOpen=false;
      }
catch (      IOException iox) {
      }
    }
  }
  public void shutdown(){
    try {
      _wantedOpen=false;
      this._provider.close();
    }
 catch (    IOException ex) {
    }
  }
}
