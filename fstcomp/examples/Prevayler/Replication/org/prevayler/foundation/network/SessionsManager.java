package org.prevayler.foundation.network;

/** 
 * Useful class comments should go here
 * $Revision: 1.1 $
 * $Date: 2010-03-05 22:04:27 $
 * $Author: apel $
 */
public interface SessionsManager {
  StubbornNetworkProxy find(  NetworkSessionId sessionId);
  NetworkSessionId add(  StubbornNetworkProxy receiver);
  void remove(  NetworkSessionId sessionId);
}
