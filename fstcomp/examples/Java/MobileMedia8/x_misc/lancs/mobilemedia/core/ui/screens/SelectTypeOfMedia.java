package lancs.mobilemedia.core.ui.screens;
import javax.microedition.lcdui.Choice;
import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.List;
import de.ovgu.cide.jakutil.*;
public class SelectTypeOfMedia extends List {
  public static final Command exitCommand=new Command("Exit",Command.STOP,2);
  public static final Command selectCommand=new Command("Select",Command.ITEM,1);
  /** 
 * Constructor
 */
  public SelectTypeOfMedia(){
    super("Select the media to Use",Choice.IMPLICIT);
  }
  /** 
 * Initialize the menu items for this screen
 */
  public void initMenu(){
    this.addCommand(exitCommand);
    this.addCommand(selectCommand);
    repaintListMedias();
  }
  public void deleteAll(){
    for (int i=0; i < this.size(); i++) {
      this.delete(i);
    }
  }
  /** 
 * @param names
 */
  public void repaintListMedias(){
    this.deleteAll();
    this.hook60();
    this.hook59();
  }
  protected void hook59(){
  }
  protected void hook60(){
  }
}
