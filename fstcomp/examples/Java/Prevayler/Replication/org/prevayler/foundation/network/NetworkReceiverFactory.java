package org.prevayler.foundation.network;
import java.io.IOException;

/** 
 * Useful class comments should go here
 * $Revision: 1.1 $
 * $Date: 2010-03-29 20:19:32 $
 * $Author: apel $
 */
public interface NetworkReceiverFactory {
  ObjectReceiver newReceiver(  Service service,  ObjectSocket socket) throws IOException ;
}
