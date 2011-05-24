package lancs.mobilemedia.core.ui.screens;
public class MediaListScreen {
  public static final Command capturePhotoCommand=new Command("Capture Photo",Command.ITEM,1);
  protected void hook48(){
    if (typeOfScreen == SHOWPHOTO) {
      this.addCommand(capturePhotoCommand);
    }
    original();
  }
}
