package lancs.mobilemedia.core.ui.screens;
public class MediaListScreen {
  public static final Command playCommand=new Command("Play",Command.ITEM,1);
  protected void hook51(){
    if (typeOfScreen == PLAYMUSIC)     this.addCommand(playCommand);
    original();
  }
}
