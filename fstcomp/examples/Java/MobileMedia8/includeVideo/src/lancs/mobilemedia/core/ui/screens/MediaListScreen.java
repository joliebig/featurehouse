package lancs.mobilemedia.core.ui.screens;
public class MediaListScreen {
  public static final Command playVideoCommand=new Command("Play Video",Command.ITEM,1);
  protected void hook53(){
    if (typeOfScreen == PLAYVIDEO) {
      this.addCommand(playVideoCommand);
    }
    original();
  }
}
