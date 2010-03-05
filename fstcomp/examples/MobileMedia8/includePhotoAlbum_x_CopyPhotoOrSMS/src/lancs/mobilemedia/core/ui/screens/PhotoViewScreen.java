package lancs.mobilemedia.core.ui.screens;
public class PhotoViewScreen {
  public static final Command copyCommand=new Command("Copy",Command.ITEM,1);
  /** 
 * Constructor
 * @param img
 */
  PhotoViewScreen(  Image img){
    this.addCommand(copyCommand);
  }
}
