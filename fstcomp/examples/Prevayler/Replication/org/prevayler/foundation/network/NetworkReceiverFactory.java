package org.prevayler.foundation.network;
import java.io.IOException;

/** 
 * Useful class comments should go here
 * $Revision: 1.1 $
 * $Date: 2010-03-05 22:04:26 $
 * $Author: apel $
 */
public interface NetworkReceiverFactory {
  ObjectReceiver newReceiver(  Service service,  ObjectSocket socket) throws IOException ;
}
