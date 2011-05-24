package lancs.mobilemedia.core.ui.screens;
import javax.microedition.lcdui.Choice;
import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.List;
import de.ovgu.cide.jakutil.*;
/** 
 * @author trevor
 * This screen displays a list of photo albums available to select.
 * A user can also create a new album on this screen.
 */
public class AlbumListScreen extends List {
  public static final Command selectCommand=new Command("Select",Command.ITEM,1);
  public static final Command createAlbumCommand=new Command("New Album",Command.ITEM,1);
  public static final Command deleteAlbumCommand=new Command("Delete Album",Command.ITEM,1);
  public static final Command resetCommand=new Command("Reset",Command.ITEM,1);
  /** 
 * Constructor
 */
  public AlbumListScreen(){
    super("Select Album",Choice.IMPLICIT);
  }
  /** 
 * Initialize the menu items for this screen
 */
  public void initMenu(){
    this.addCommand(exitCommand);
    this.addCommand(selectCommand);
    this.addCommand(createAlbumCommand);
    this.addCommand(deleteAlbumCommand);
    this.hook45();
    this.addCommand(resetCommand);
  }
  public void deleteAll(){
    for (int i=0; i < this.size(); i++) {
      this.delete(i);
    }
  }
  /** 
 * @param names
 */
  public void repaintListAlbum(  String[] names){
    String[] albumNames=names;
    this.deleteAll();
    for (int i=0; i < albumNames.length; i++) {
      if (albumNames[i] != null) {
        this.append(albumNames[i],null);
      }
    }
  }
  protected void hook45(){
  }
}
