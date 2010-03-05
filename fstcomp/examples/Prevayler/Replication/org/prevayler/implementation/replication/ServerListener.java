package org.prevayler.implementation.replication;
import org.prevayler.foundation.network.OldNetwork;
import org.prevayler.foundation.network.ObjectServerSocket;
import org.prevayler.implementation.publishing.TransactionPublisher;
import java.io.IOException;

/** 
 * Reserved for future implementation.
 */
public class ServerListener extends Thread {
  private final TransactionPublisher _publisher;
  private final ObjectServerSocket _serverSocket;
  public ServerListener(  TransactionPublisher publisher,  OldNetwork network,  int port) throws IOException {
    _serverSocket=network.openObjectServerSocket(port);
    _publisher=publisher;
    setDaemon(true);
    start();
  }
  public void run(){
    try {
      while (true)       new ServerConnection(_publisher,_serverSocket.accept());
    }
 catch (    IOException iox) {
      iox.printStackTrace();
    }
  }
}
