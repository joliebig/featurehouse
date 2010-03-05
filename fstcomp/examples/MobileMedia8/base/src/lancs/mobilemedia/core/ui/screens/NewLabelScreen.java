package lancs.mobilemedia.core.ui.screens;
import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.Form;
import javax.microedition.lcdui.TextField;
import de.ovgu.cide.jakutil.*;
/** 
 * @author tyoung
 * This form is displayed when the user defines a new photo album (from the AlbumListScreen)
 * TODO: This feature is not fully implemented. The commands are in place to create
 * a new album, but currently it just creates a hard-coded album name for testing.
 */
public class NewLabelScreen extends Form {
  public static final int NEW_ALBUM=0;
  public static final int LABEL_PHOTO=1;
  TextField labelName=new TextField("Name","",15,TextField.ANY);
  Command ok;
  Command cancel;
  private int formType;
  /** 
 * @param arg0
 */
  public NewLabelScreen(  String name,  int type){
    super(name);
    this.formType=type;
    this.append(labelName);
    this.hook54();
    ok=new Command("Save",Command.SCREEN,0);
    cancel=new Command("Cancel",Command.EXIT,1);
    this.addCommand(ok);
    this.addCommand(cancel);
  }
  /** 
 * @return Returns the new Album Name.
 */
  public String getLabelName(){
    return labelName.getString();
  }
  /** 
 * @param formType the formType to set
 */
  public void setFormType(  int formType){
    this.formType=formType;
  }
  /** 
 * @return the formType
 */
  public int getFormType(){
    return formType;
  }
  protected void hook54(){
  }
}
