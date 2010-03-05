package org.prevayler;
import org.prevayler.foundation.network.OldNetwork;
import org.prevayler.foundation.network.OldNetworkImpl;
import org.prevayler.implementation.replication.ClientPublisher;
import org.prevayler.implementation.replication.ServerListener;
public class PrevaylerFactory {
  private OldNetwork _network;
  private int _serverPort=-1;
  private String _remoteServerIpAddress;
  private int _remoteServerPort;
  public static final int DEFAULT_REPLICATION_PORT=8756;
  /** 
 * Reserved for future implementation.
 */
  public void configureReplicationClient(  String remoteServerIpAddress,  int remoteServerPort){
    _remoteServerIpAddress=remoteServerIpAddress;
    _remoteServerPort=remoteServerPort;
  }
  /** 
 * Reserved for future implementation.
 */
  public void configureReplicationServer(  int port){
    _serverPort=port;
  }
  public void configureNetwork(  OldNetwork network){
    _network=network;
  }
  private OldNetwork network(){
    return _network != null ? _network : new OldNetworkImpl();
  }
  protected void hook67() throws IOException {
    if (_remoteServerIpAddress != null)     throw new ReturnObject(new ClientPublisher(network(),_remoteServerIpAddress,_remoteServerPort));
    original();
  }
@MethodObject static class PrevaylerFactory_create {
    protected void hook68() throws IOException, ClassNotFoundException {
      if (_this._serverPort != -1)       new ServerListener(publisher,_this.network(),_this._serverPort);
      original();
    }
  }
}
