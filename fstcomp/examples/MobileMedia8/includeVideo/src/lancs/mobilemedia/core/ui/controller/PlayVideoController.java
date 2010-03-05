package lancs.mobilemedia.core.ui.controller;
import javax.microedition.lcdui.Alert;
import javax.microedition.lcdui.AlertType;
import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.Display;
import javax.microedition.lcdui.List;
import javax.microedition.rms.RecordStoreFullException;
import lancs.mobilemedia.core.ui.MainUIMidlet;
import lancs.mobilemedia.core.ui.datamodel.AlbumData;
import lancs.mobilemedia.core.ui.datamodel.MediaData;
import lancs.mobilemedia.core.ui.screens.AddMediaToAlbum;
import lancs.mobilemedia.core.ui.screens.AlbumListScreen;
import lancs.mobilemedia.core.ui.screens.PlayVideoScreen;
import lancs.mobilemedia.core.util.Constants;
import lancs.mobilemedia.lib.exceptions.ImageNotFoundException;
import lancs.mobilemedia.lib.exceptions.ImagePathNotValidException;
import lancs.mobilemedia.lib.exceptions.InvalidImageDataException;
import lancs.mobilemedia.lib.exceptions.PersistenceMechanismException;
import de.ovgu.cide.jakutil.*;
public class PlayVideoController extends AbstractController {
  private PlayVideoScreen pmscreen;
  public PlayVideoController(  MainUIMidlet midlet,  AlbumData albumData,  List albumListScreen,  PlayVideoScreen pmscreen){
    super(midlet,albumData,albumListScreen);
    this.pmscreen=pmscreen;
  }
  public boolean handleCommand(  Command command){
    return new PlayVideoController_handleCommand(this,command).execute();
  }
@MethodObject static class PlayVideoController_handleCommand {
    PlayVideoController_handleCommand(    PlayVideoController _this,    Command command){
      this._this=_this;
      this.command=command;
    }
    boolean execute(){
      try {
        label=command.getLabel();
        System.out.println("<* PlayVideoController.handleCommand() *> " + label);
        if (label.equals("Start")) {
          _this.pmscreen.startVideo();
          return true;
        }
        if (label.equals("Stop")) {
          _this.pmscreen.stopVideo();
          return true;
        }
        if ((label.equals("Back")) || (label.equals("Cancel"))) {
          _this.pmscreen.stopVideo();
          ((AlbumListScreen)_this.getAlbumListScreen()).repaintListAlbum(_this.getAlbumData().getAlbumNames());
          _this.setCurrentScreen(_this.getAlbumListScreen());
          ScreenSingleton.getInstance().setCurrentScreenName(Constants.ALBUMLIST_SCREEN);
          return true;
        }
        this.hook40();
        return false;
      }
 catch (      ReturnBoolean r) {
        return r.value;
      }
    }
    protected PlayVideoController _this;
    protected Command command;
    protected String label;
    protected AddMediaToAlbum copyPhotoToAlbum;
    protected MediaData imageData;
    protected Alert alert;
    protected String albumname;
    protected Alert alert;
    protected Alert alert;
    protected void hook40(){
    }
  }
}
