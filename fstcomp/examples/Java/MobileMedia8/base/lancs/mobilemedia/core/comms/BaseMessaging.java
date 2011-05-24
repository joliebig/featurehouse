package lancs.mobilemedia.core.comms;
import java.io.IOException;
import java.io.InterruptedIOException;
import javax.wireless.messaging.MessageConnection;
import de.ovgu.cide.jakutil.*;
/** 
 * @author tyoung
 * This is the abstract base messaging class. (At the moment, no messaging classes are implemented)
 * It is used for communication between the device and a network. Generally to upload or 
 * download data such as photos to the application. Currently, the prototype just loads data
 * from the jar file directly.
 * Specific messaging classes that use different protocols should implement this class 
 * (ie. SMS messaging, HTTP messaging, Infrared, Bluetooth etc.)
 */
public abstract class BaseMessaging {
  public abstract boolean sendImage(  byte[] imageData);
  public abstract byte[] receiveImage() throws InterruptedIOException, IOException ;
  public abstract void cleanUpConnections(  MessageConnection conn);
}
