package lancs.mobilemedia.core.ui.screens;
public class MediaListScreen {
  public static final Command captureVideoCommand=new Command("Capture Video",Command.ITEM,1);
  protected void hook49(){
    if (typeOfScreen == PLAYVIDEO) {
      this.addCommand(captureVideoCommand);
    }
    original();
  }
}
