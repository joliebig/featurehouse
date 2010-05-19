package lancs.mobilemedia.core.ui.screens;
public class MediaListScreen {
  public static final Command viewCommand=new Command("View",Command.ITEM,1);
  /** 
 * Initialize the menu items for this screen
 */
  public void initMenu(){
    if (typeOfScreen == SHOWPHOTO)     this.addCommand(viewCommand);
    original();
  }
}
