package lancs.mobilemedia.core.ui.screens;
import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.Form;
import javax.microedition.lcdui.TextField;
import de.ovgu.cide.jakutil.*;
public class PasswordScreen extends Form {
  TextField labelPassword=new TextField("Password","",15,TextField.PASSWORD);
  Command ok;
  Command cancel;
  public PasswordScreen(  String name,  int type){
    super(name);
    this.append(labelPassword);
    if (type == 0)     ok=new Command("Save",Command.SCREEN,0);
 else     if (type == 1)     ok=new Command("Confirm",Command.SCREEN,0);
 else     if (type == 3)     ok=new Command("Store",Command.SCREEN,0);
    cancel=new Command("Cancel",Command.EXIT,1);
    this.addCommand(ok);
    this.addCommand(cancel);
  }
  public String getPassword(){
    return this.labelPassword.getString();
  }
  public void setPassword(  String newPassword){
    this.labelPassword.setString(newPassword);
  }
}
