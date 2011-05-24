package lancs.mobilemedia.core.ui.screens;
public class MediaListScreen {
  public static final Command sortCommand=new Command("Sort by Views",Command.ITEM,1);
  protected void hook52(){
    this.addCommand(sortCommand);
    original();
  }
}
