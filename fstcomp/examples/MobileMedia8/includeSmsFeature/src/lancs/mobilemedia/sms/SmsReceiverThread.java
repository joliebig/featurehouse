package lancs.mobilemedia.sms;
import java.io.IOException;
import java.io.InterruptedIOException;
import javax.microedition.lcdui.Alert;
import javax.microedition.lcdui.Command;
import javax.wireless.messaging.Message;
import javax.wireless.messaging.MessageConnection;
import lancs.mobilemedia.core.ui.MainUIMidlet;
import lancs.mobilemedia.core.ui.datamodel.AlbumData;
import lancs.mobilemedia.core.ui.screens.AlbumListScreen;
import de.ovgu.cide.jakutil.*;
/** 
 */
public class SmsReceiverThread implements Runnable {
  SmsReceiverController controller=null;
  String[] connections;
  String smsPort;
  MessageConnection smsconn;
  Message msg;
  String senderAddress;
  Command acceptPhotoCommand=new Command("Accept Photo",Command.ITEM,1);
  Command rejectPhotoCommand=new Command("Reject Photo",Command.ITEM,1);
  Command errorNotice=new Command("Ok",Command.ITEM,1);
  /** 
 * Initialize the MIDlet with the current display object and graphical
 * components.
 */
  public SmsReceiverThread(  MainUIMidlet midlet,  AlbumData albumData,  AlbumListScreen albumListScreen,  SmsReceiverController controller){
    this.controller=controller;
    smsPort="1000";
  }
  /** 
 * Message reading thread. 
 */
  public void run(){
    SmsMessaging smsMessenger=new SmsMessaging();
    while (true) {
      System.out.println("Starting SMSReceiver::run()");
      smsMessenger.setSmsReceivePort(smsPort);
      byte[] receivedData=null;
      try {
        receivedData=smsMessenger.receiveImage();
      }
 catch (      InterruptedIOException e) {
        Alert alert=new Alert("Error Incoming Photo");
        alert.setString("" + "You have just received a bad fragmentated photo which was not possible to recovery.");
        alert.addCommand(errorNotice);
        System.out.println("Error interreput");
        alert.setCommandListener(controller);
        controller.setCurrentScreen(alert);
        smsMessenger.cleanUpReceiverConnections();
        continue;
      }
catch (      IOException e) {
        Alert alert=new Alert("Error Incoming Photo");
        alert.setString("" + "You have just received a bad fragmentated photo which was not possible to recovery.");
        alert.addCommand(errorNotice);
        System.out.println("Bad fragmentation");
        alert.setCommandListener(controller);
        controller.setCurrentScreen(alert);
        smsMessenger.cleanUpReceiverConnections();
        continue;
      }
      System.out.println("BEFORE ALERT CODE");
      Alert alert=new Alert("New Incoming Photo");
      alert.setString("A MobilePhoto user has sent you a Photo. Do you want to accept it?");
      alert.addCommand(acceptPhotoCommand);
      alert.addCommand(rejectPhotoCommand);
      controller.setIncommingData(receivedData);
      alert.setCommandListener(controller);
      controller.setCurrentScreen(alert);
      System.out.println("Finishing SMSReceiver run()");
    }
  }
}
