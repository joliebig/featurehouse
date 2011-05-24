package org.prevayler.foundation.network;

/** 
 * Useful class comments should go here
 * $Revision: 1.1 $
 * $Date: 2010-03-29 20:19:32 $
 * $Author: apel $
 */
public interface SessionsManager {
  StubbornNetworkProxy find(  NetworkSessionId sessionId);
  NetworkSessionId add(  StubbornNetworkProxy receiver);
  void remove(  NetworkSessionId sessionId);
}
