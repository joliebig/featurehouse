package lancs.mobilemedia.sms;
import de.ovgu.cide.jakutil.*;
/** 
 * Prompts for text and sends it via an SMS MessageConnection
 */
public class SmsSenderThread implements Runnable {
  private String smsPort;
  /** 
 * The URL to send the message to 
 */
  private String destinationAddress;
  private String messageText="default";
  private byte[] binData;
  public SmsSenderThread(  String smsPort,  String destinationAddress,  String messageText){
    System.out.println("SmsSenderThread:: 3 Param Constructor: " + smsPort + ","+ destinationAddress+ ","+ messageText);
    this.messageText=messageText;
    this.destinationAddress=destinationAddress;
    this.smsPort=smsPort;
  }
  /** 
 * Send the message. Called on a separate thread so we don't have
 * contention for the display
 */
  public void run(){
    System.out.println("SmsSenderThread::run: Sending message: " + messageText + " to: "+ destinationAddress);
    SmsMessaging smsMessenger=new SmsMessaging(smsPort,destinationAddress);
    smsMessenger.sendImage(this.binData);
    System.out.println("Finishing SMSSender run()");
  }
  /** 
 * @return Returns the messageText.
 */
  public String getMessageText(){
    return messageText;
  }
  /** 
 * @param messageText The messageText to set.
 */
  public void setMessageText(  String messageText){
    this.messageText=messageText;
  }
  public void setBinaryData(  byte[] data){
    System.out.println("SmsSenderThread: setBinaryData of length: " + data.length);
    this.binData=data;
  }
  /** 
 * @return Returns the smsPort.
 */
  public String getSmsPort(){
    return smsPort;
  }
  /** 
 * @param smsPort The smsPort to set.
 */
  public void setSmsPort(  String smsPort){
    this.smsPort=smsPort;
  }
}
