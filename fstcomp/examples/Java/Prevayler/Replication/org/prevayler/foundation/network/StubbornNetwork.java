package org.prevayler.foundation.network;
import java.io.IOException;

/** 
 * Useful class comments should go here
 * $Revision: 1.1 $
 * $Date: 2010-03-29 20:19:31 $
 * $Author: apel $
 */
public interface StubbornNetwork {
  ObjectSocket newInstance(  String ipAddress,  int port) throws IOException ;
}
