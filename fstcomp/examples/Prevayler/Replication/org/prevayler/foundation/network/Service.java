package org.prevayler.foundation.network;

public interface Service {
  ObjectReceiver serverFor(  ObjectReceiver client);
}
