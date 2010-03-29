package lancs.mobilemedia.core.ui.screens;
import javax.microedition.lcdui.Choice;
import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.List;
import de.ovgu.cide.jakutil.*;
/** 
 * @author trevor
 * This screen shows a listing of all photos for a selected photo album.
 * This is the screen that contains most of the feature menu items. 
 * From this screen, a user can choose to view photos, add or delete photos,
 * send photos to other users etc.
 */
public class MediaListScreen extends List {
  public static final int SHOWPHOTO=1;
  public static final int PLAYMUSIC=2;
  public static final int PLAYVIDEO=3;
  public static final Command addCommand=new Command("Add",Command.ITEM,1);
  public static final Command deleteCommand=new Command("Delete",Command.ITEM,1);
  public static final Command backCommand=new Command("Back",Command.BACK,0);
  public static final Command editLabelCommand=new Command("Edit Label",Command.ITEM,1);
  /** 
 * Constructor
 */
  private int typeOfScreen;
  public MediaListScreen(  int typeOfScreen){
    super("Choose Items",Choice.IMPLICIT);
    this.typeOfScreen=typeOfScreen;
  }
  /** 
 * Initialize the menu items for this screen
 */
  public void initMenu(){
    this.hook51();
    this.hook53();
    this.hook49();
    this.hook48();
    this.addCommand(addCommand);
    this.addCommand(deleteCommand);
    this.addCommand(editLabelCommand);
    this.hook52();
    this.hook50();
    this.addCommand(backCommand);
  }
  protected void hook48(){
  }
  protected void hook49(){
  }
  protected void hook50(){
  }
  protected void hook51(){
  }
  protected void hook52(){
  }
  protected void hook53(){
  }
}
