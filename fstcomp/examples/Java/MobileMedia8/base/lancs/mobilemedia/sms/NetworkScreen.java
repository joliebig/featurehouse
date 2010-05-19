package lancs.mobilemedia.sms;
import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.Form;
import javax.microedition.lcdui.TextField;
import de.ovgu.cide.jakutil.*;
/** 
 * @author tyoung
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class NetworkScreen extends Form {
  TextField recPhoneNum=new TextField("Phone #","5550001",15,TextField.ANY);
  String rPort="1000";
  Command ok;
  Command cancel;
  /** 
 * @param arg0
 */
  public NetworkScreen(  String title){
    super(title);
    recPhoneNum.setString("5550001");
    this.append(recPhoneNum);
    ok=new Command("Send Now",Command.OK,0);
    cancel=new Command("Cancel",Command.EXIT,1);
    this.addCommand(ok);
    this.addCommand(cancel);
  }
  /** 
 * @return Returns the recPhoneNum.
 */
  public String getRecPhoneNum(){
    return recPhoneNum.getString();
  }
  /** 
 * @return Returns the recPort.
 */
  public String getRecPort(){
    return rPort;
  }
}
