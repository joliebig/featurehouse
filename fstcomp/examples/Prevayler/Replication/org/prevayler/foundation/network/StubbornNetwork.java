package org.prevayler.foundation.network;
import java.io.IOException;

/** 
 * Useful class comments should go here
 * $Revision: 1.1 $
 * $Date: 2010-03-05 22:04:25 $
 * $Author: apel $
 */
public interface StubbornNetwork {
  ObjectSocket newInstance(  String ipAddress,  int port) throws IOException ;
}
