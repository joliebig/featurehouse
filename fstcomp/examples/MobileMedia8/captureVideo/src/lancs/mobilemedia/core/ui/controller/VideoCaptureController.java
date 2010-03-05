package lancs.mobilemedia.core.ui.controller;
import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.Display;
import javax.microedition.lcdui.List;
import lancs.mobilemedia.core.ui.MainUIMidlet;
import lancs.mobilemedia.core.ui.datamodel.AlbumData;
import lancs.mobilemedia.core.ui.screens.AddMediaToAlbum;
import lancs.mobilemedia.core.ui.screens.AlbumListScreen;
import lancs.mobilemedia.core.ui.screens.CaptureVideoScreen;
import lancs.mobilemedia.core.util.Constants;
import lancs.mobilemedia.lib.exceptions.InvalidImageDataException;
import lancs.mobilemedia.lib.exceptions.PersistenceMechanismException;
import de.ovgu.cide.jakutil.*;
public class VideoCaptureController extends AbstractController {
  private CaptureVideoScreen pmscreen;
  private AddMediaToAlbum saveVideoToAlbum;
  public VideoCaptureController(  MainUIMidlet midlet,  AlbumData albumData,  List albumListScreen,  CaptureVideoScreen pmscreen){
    super(midlet,albumData,albumListScreen);
    this.pmscreen=pmscreen;
  }
  public boolean handleCommand(  Command command){
    String label=command.getLabel();
    System.out.println("<* VideoCaptureController.handleCommand() *> " + label);
    if (label.equals("Start")) {
      pmscreen.startCapture();
      return true;
    }
 else     if (label.equals("Stop")) {
      pmscreen.pauseCapture();
      saveVideoToAlbum=new AddMediaToAlbum("Save Video");
      saveVideoToAlbum.setItemName("Capture video");
      saveVideoToAlbum.setLabePath("Save to Album:");
      saveVideoToAlbum.setCommandListener(this);
      saveVideoToAlbum.setCapturedMedia(pmscreen.getByteArrays());
      Display.getDisplay(midlet).setCurrent(saveVideoToAlbum);
      return true;
    }
 else     if (label.equals("Save Item")) {
      String videoname=((AddMediaToAlbum)getCurrentScreen()).getItemName();
      String albumname=((AddMediaToAlbum)getCurrentScreen()).getPath();
      try {
        getAlbumData().addVideoData(videoname,albumname,saveVideoToAlbum.getCapturedMedia());
      }
 catch (      InvalidImageDataException e) {
        e.printStackTrace();
      }
catch (      PersistenceMechanismException e) {
        e.printStackTrace();
      }
    }
 else     if ((label.equals("Back")) || (label.equals("Cancel"))) {
      pmscreen.pauseCapture();
      ((AlbumListScreen)getAlbumListScreen()).repaintListAlbum(getAlbumData().getAlbumNames());
      setCurrentScreen(getAlbumListScreen());
      ScreenSingleton.getInstance().setCurrentScreenName(Constants.ALBUMLIST_SCREEN);
      return true;
    }
    return false;
  }
}
