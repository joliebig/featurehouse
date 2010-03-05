package org.prevayler.foundation.network;
import java.io.IOException;

/** 
 * Provides a Basic Network Service, no recovery of failure, and no
 * reconnect support.
 */
public class NetworkImpl extends BaseNetworkImpl {
  public ObjectReceiver newReceiver(  String ipAddress,  int port,  ObjectReceiver client) throws IOException {
    return new NetworkClientObjectReceiverImpl(ipAddress,port,client);
  }
  public ObjectReceiver newReceiver(  Service service,  ObjectSocket socket) throws IOException {
    return new NetworkClientObjectReceiverImpl(socket,service);
  }
}
