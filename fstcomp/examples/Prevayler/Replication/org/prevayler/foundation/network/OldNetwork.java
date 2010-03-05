package org.prevayler.foundation.network;
import java.io.IOException;

public interface OldNetwork {
  ObjectSocket openSocket(  String serverIpAddress,  int serverPort) throws IOException ;
  ObjectServerSocket openObjectServerSocket(  int port) throws IOException ;
}
