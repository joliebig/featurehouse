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
import lancs.mobilemedia.core.ui.screens.PlayMediaScreen;
import lancs.mobilemedia.core.util.Constants;
import lancs.mobilemedia.lib.exceptions.ImageNotFoundException;
import lancs.mobilemedia.lib.exceptions.ImagePathNotValidException;
import lancs.mobilemedia.lib.exceptions.InvalidImageDataException;
import lancs.mobilemedia.lib.exceptions.PersistenceMechanismException;
import de.ovgu.cide.jakutil.*;
public class MusicPlayController extends AbstractController {
  private PlayMediaScreen pmscreen;
  public MusicPlayController(  MainUIMidlet midlet,  AlbumData albumData,  List albumListScreen,  PlayMediaScreen pmscreen){
    super(midlet,albumData,albumListScreen);
    this.pmscreen=pmscreen;
  }
  public boolean handleCommand(  Command command){
    return new MusicPlayController_handleCommand(this,command).execute();
  }
@MethodObject static class MusicPlayController_handleCommand {
    MusicPlayController_handleCommand(    MusicPlayController _this,    Command command){
      this._this=_this;
      this.command=command;
    }
    boolean execute(){
      try {
        label=command.getLabel();
        System.out.println("<* MusicPlayController.handleCommand() *> " + label);
        if (label.equals("Start")) {
          _this.pmscreen.startPlay();
          return true;
        }
        if (label.equals("Stop")) {
          _this.pmscreen.pausePlay();
          return true;
        }
        if ((label.equals("Back")) || (label.equals("Cancel"))) {
          _this.pmscreen.pausePlay();
          ((AlbumListScreen)_this.getAlbumListScreen()).repaintListAlbum(_this.getAlbumData().getAlbumNames());
          _this.setCurrentScreen(_this.getAlbumListScreen());
          ScreenSingleton.getInstance().setCurrentScreenName(Constants.ALBUMLIST_SCREEN);
          return true;
        }
        this.hook35();
        return false;
      }
 catch (      ReturnBoolean r) {
        return r.value;
      }
    }
    protected MusicPlayController _this;
    protected Command command;
    protected String label;
    protected AddMediaToAlbum copyPhotoToAlbum;
    protected MediaData imageData;
    protected Alert alert;
    protected String albumname;
    protected Alert alert;
    protected Alert alert;
    protected void hook35(){
    }
  }
}
