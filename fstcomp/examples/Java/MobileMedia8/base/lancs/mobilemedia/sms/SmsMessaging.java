package lancs.mobilemedia.sms;
import java.io.IOException;
import java.io.InterruptedIOException;
import javax.microedition.io.Connector;
import javax.microedition.io.PushRegistry;
import javax.wireless.messaging.BinaryMessage;
import javax.wireless.messaging.Message;
import javax.wireless.messaging.MessageConnection;
import javax.wireless.messaging.TextMessage;
import lancs.mobilemedia.core.comms.BaseMessaging;
import de.ovgu.cide.jakutil.*;
/** 
 * @author trevor
 * Insert Comments here
 */
public class SmsMessaging extends BaseMessaging {
  private String smsSendPort;
  private String smsReceivePort="1000";
  private String destinationPhoneNumber;
  private String smsProtocolPrefix="sms://";
  private MessageConnection smsConn=null;
  private Message msg;
  private String[] connections;
  public SmsMessaging(){
    smsSendPort="1000";
    smsReceivePort="1000";
  }
  public SmsMessaging(  String smsDstPort,  String destinationPhoneNumber){
    this.destinationPhoneNumber=destinationPhoneNumber;
    this.smsSendPort=smsDstPort;
  }
  public boolean sendImage(  byte[] imageData){
    boolean success=true;
    String address=destinationPhoneNumber;
    if ((smsSendPort != null) && (smsSendPort != ""))     address=smsProtocolPrefix + address + ":"+ smsSendPort+ 1;
    System.out.println("SmsMessaging::sendImage: Sending binary message to: " + address);
    MessageConnection smsconn=null;
    try {
      smsconn=(MessageConnection)Connector.open(address);
      BinaryMessage binmsg=(BinaryMessage)smsconn.newMessage(MessageConnection.BINARY_MESSAGE);
      binmsg.setPayloadData(imageData);
      int i=smsconn.numberOfSegments(binmsg);
      System.out.println("SmsMessaging::sendImage() num segments to send is: " + i);
      smsconn.send(binmsg);
    }
 catch (    Throwable t) {
      System.out.println("Send caught: ");
      t.printStackTrace();
      return false;
    }
    cleanUpConnections(smsconn);
    return success;
  }
  public byte[] receiveImage() throws InterruptedIOException, IOException {
    System.out.println("SmsMessaging::receiveImage() - start");
    byte[] receivedData=null;
    String smsConnection=smsProtocolPrefix + ":" + smsReceivePort;
    String senderAddress;
    if (smsConn == null) {
      try {
        smsConn=(MessageConnection)Connector.open(smsConnection);
      }
 catch (      IOException ioe) {
        ioe.printStackTrace();
      }
    }
    connections=PushRegistry.listConnections(true);
    if (connections == null || connections.length == 0) {
      System.out.println("Waiting for SMS on " + smsConnection + "...");
    }
    System.out.println("DEBUG 1: before smsConn.receive():" + smsConn);
    msg=smsConn.receive();
    System.out.println("DEBUG 2: after smsConn.receive()");
    if (msg != null) {
      senderAddress=msg.getAddress();
      System.out.println("From: " + senderAddress);
      if (msg instanceof TextMessage) {
        String incomingMessage=((TextMessage)msg).getPayloadText();
        System.out.println("Incoming SMS Message with Payload:" + incomingMessage);
      }
 else {
        System.out.println("Incoming Binary SMS Message...");
        StringBuffer buf=new StringBuffer();
        receivedData=((BinaryMessage)msg).getPayloadData();
        System.out.println("SmsMessaging::receiveImage: sender address = " + senderAddress.toString());
        System.out.println("SmsMessaging::receiveImage: buffer length = " + buf.length() + " contents = "+ buf.toString());
      }
    }
    System.out.println("SmsMessaging::receiveImage() - Finish");
    return receivedData;
  }
  public void cleanUpConnections(  MessageConnection smsConn){
    if (smsConn != null) {
      try {
        smsConn.close();
      }
 catch (      IOException ioe) {
        System.out.println("Closing connection caught: ");
        ioe.printStackTrace();
      }
    }
  }
  public void cleanUpReceiverConnections(){
    if (smsConn != null) {
      try {
        smsConn.close();
        smsConn=null;
      }
 catch (      IOException ioe) {
        System.out.println("Closing connection caught: ");
        ioe.printStackTrace();
      }
    }
  }
  /** 
 * @return Returns the destinationPhoneNumber.
 */
  public String getDestinationPhoneNumber(){
    return destinationPhoneNumber;
  }
  /** 
 * @param destinationPhoneNumber The destinationPhoneNumber to set.
 */
  public void setDestinationPhoneNumber(  String destinationPhoneNumber){
    this.destinationPhoneNumber=destinationPhoneNumber;
  }
  /** 
 * @return Returns the smsReceivePort.
 */
  public String getSmsReceivePort(){
    return smsReceivePort;
  }
  /** 
 * @param smsReceivePort The smsReceivePort to set.
 */
  public void setSmsReceivePort(  String smsReceivePort){
    this.smsReceivePort=smsReceivePort;
  }
  /** 
 * @return Returns the smsSendPort.
 */
  public String getSmsSendPort(){
    return smsSendPort;
  }
  /** 
 * @param smsSendPort The smsSendPort to set.
 */
  public void setSmsSendPort(  String smsSendPort){
    this.smsSendPort=smsSendPort;
  }
}
