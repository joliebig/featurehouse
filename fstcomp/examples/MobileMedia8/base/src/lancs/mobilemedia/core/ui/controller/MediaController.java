package lancs.mobilemedia.core.ui.controller;
import java.io.InputStream;
import javax.microedition.lcdui.Alert;
import javax.microedition.lcdui.AlertType;
import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.Display;
import javax.microedition.lcdui.List;
import javax.microedition.rms.RecordStoreFullException;
import lancs.mobilemedia.lib.exceptions.ImageNotFoundException;
import lancs.mobilemedia.lib.exceptions.ImagePathNotValidException;
import lancs.mobilemedia.lib.exceptions.InvalidImageDataException;
import lancs.mobilemedia.lib.exceptions.PersistenceMechanismException;
import lancs.mobilemedia.core.ui.MainUIMidlet;
import lancs.mobilemedia.core.ui.datamodel.AlbumData;
import lancs.mobilemedia.core.ui.datamodel.MediaData;
import lancs.mobilemedia.core.ui.screens.AddMediaToAlbum;
import lancs.mobilemedia.core.ui.screens.AlbumListScreen;
import lancs.mobilemedia.core.ui.screens.NewLabelScreen;
import lancs.mobilemedia.core.util.Constants;
import de.ovgu.cide.jakutil.*;
/** 
 * @author Eduardo Figueiredo
 * Added in the Scenario 02
 */
public class MediaController extends MediaListController {
  private MediaData media;
  private NewLabelScreen screen;
  public MediaController(  MainUIMidlet midlet,  AlbumData albumData,  AlbumListScreen albumListScreen){
    super(midlet,albumData,albumListScreen);
  }
  public boolean handleCommand(  Command command){
    return new MediaController_handleCommand(this,command).execute();
  }
  void updateMedia(  MediaData media) throws InvalidImageDataException, PersistenceMechanismException {
    getAlbumData().updateMediaInfo(media,media);
  }
  /** 
 * Get the last selected image from the Photo List screen.
 * TODO: This really only gets the selected List item. 
 * So it is only an image name if you are on the PhotoList screen...
 * Need to fix this
 */
  public String getSelectedMediaName(){
    List selected=(List)Display.getDisplay(midlet).getCurrent();
    if (selected == null)     System.out.println("Current List from display is NULL!");
    String name=selected.getString(selected.getSelectedIndex());
    return name;
  }
  /** 
 * [EF] update this method or merge with method of super class.
 * Go to the previous screen
 */
  private boolean goToPreviousScreen(){
    System.out.println("<* PhotoController.goToPreviousScreen() *>");
    String currentScreenName=ScreenSingleton.getInstance().getCurrentScreenName();
    if (currentScreenName.equals(Constants.ALBUMLIST_SCREEN)) {
      System.out.println("Can't go back here...Should never reach this spot");
    }
 else     if (currentScreenName.equals(Constants.IMAGE_SCREEN)) {
      showMediaList(getCurrentStoreName(),false,false);
      ScreenSingleton.getInstance().setCurrentScreenName(Constants.IMAGELIST_SCREEN);
      return true;
    }
 else     if (currentScreenName.equals(Constants.ADDPHOTOTOALBUM_SCREEN)) {
      showMediaList(getCurrentStoreName(),false,false);
      ScreenSingleton.getInstance().setCurrentScreenName(Constants.IMAGELIST_SCREEN);
      return true;
    }
    return false;
  }
  /** 
 * @param image the image to set
 */
  public void setMedia(  MediaData media){
    this.media=media;
  }
  /** 
 * @return the image
 */
  public MediaData getMedia(){
    return media;
  }
  /** 
 * @param screen
 */
  public void setScreen(  NewLabelScreen screen){
    this.screen=screen;
  }
  /** 
 * @return
 */
  public NewLabelScreen getScreen(){
    return screen;
  }
@MethodObject static class MediaController_handleCommand {
    MediaController_handleCommand(    MediaController _this,    Command command){
      this._this=_this;
      this.command=command;
    }
    boolean execute(){
      try {
        label=command.getLabel();
        System.out.println("<* PhotoController.handleCommand() *> " + label);
        if (label.equals("Add")) {
          ScreenSingleton.getInstance().setCurrentScreenName(Constants.ADDPHOTOTOALBUM_SCREEN);
          form=new AddMediaToAlbum("Add new item to Album");
          form.setCommandListener(_this);
          _this.setCurrentScreen(form);
          return true;
        }
        this.hook26();
        this.hook23();
        this.hook28();
        this.hook22();
        this.hook21();
        if (label.equals("Save Item")) {
          try {
            _this.getAlbumData().addNewMediaToAlbum(((AddMediaToAlbum)_this.getCurrentScreen()).getItemName(),((AddMediaToAlbum)_this.getCurrentScreen()).getPath(),_this.getCurrentStoreName());
            this.hook24();
          }
 catch (          InvalidImageDataException e) {
            alert=null;
            if (e instanceof ImagePathNotValidException)             alert=new Alert("Error","The path is not valid",null,AlertType.ERROR);
 else             alert=new Alert("Error","The file format is not valid",null,AlertType.ERROR);
            Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
            return true;
          }
catch (          PersistenceMechanismException e) {
            alert=null;
            if (e.getCause() instanceof RecordStoreFullException)             alert=new Alert("Error","The mobile database is full",null,AlertType.ERROR);
 else             alert=new Alert("Error","The mobile database can not add a new photo",null,AlertType.ERROR);
            Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
          }
catch (          ImageNotFoundException e) {
            this.hook25();
          }
          return _this.goToPreviousScreen();
        }
        if (label.equals("Delete")) {
          selectedMediaName=_this.getSelectedMediaName();
          try {
            _this.getAlbumData().deleteMedia(_this.getCurrentStoreName(),selectedMediaName);
          }
 catch (          PersistenceMechanismException e) {
            alert=new Alert("Error","The mobile database can not delete this item",null,AlertType.ERROR);
            Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
            return true;
          }
catch (          ImageNotFoundException e) {
            alert=new Alert("Error","The selected item was not found in the mobile device",null,AlertType.ERROR);
            Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
            return true;
          }
          _this.showMediaList(_this.getCurrentStoreName(),false,false);
          ScreenSingleton.getInstance().setCurrentScreenName(Constants.IMAGELIST_SCREEN);
          return true;
        }
        if (label.equals("Edit Label")) {
          selectedImageName=_this.getSelectedMediaName();
          try {
            _this.media=_this.getAlbumData().getMediaInfo(selectedImageName);
            formScreen=new NewLabelScreen("Edit Label Item",NewLabelScreen.LABEL_PHOTO);
            formScreen.setCommandListener(_this);
            _this.setScreen(formScreen);
            _this.setCurrentScreen(formScreen);
            formScreen=null;
          }
 catch (          ImageNotFoundException e) {
            alert=new Alert("Error","The selected item was not found in the mobile device",null,AlertType.ERROR);
            Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
          }
          return true;
        }
        this.hook27();
        if (label.equals("Set Favorite")) {
          selectedMediaName=_this.getSelectedMediaName();
          try {
            media=_this.getAlbumData().getMediaInfo(selectedMediaName);
            media.toggleFavorite();
            _this.updateMedia(media);
            System.out.println("<* BaseController.handleCommand() *> Image = " + selectedMediaName + "; Favorite = "+ media.isFavorite());
          }
 catch (          ImageNotFoundException e) {
            alert=new Alert("Error","The selected item was not found in the mobile device",null,AlertType.ERROR);
            Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
          }
catch (          InvalidImageDataException e) {
            alert=null;
            if (e instanceof ImagePathNotValidException)             alert=new Alert("Error","The path is not valid",null,AlertType.ERROR);
 else             alert=new Alert("Error","The image file format is not valid",null,AlertType.ERROR);
            Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
          }
catch (          PersistenceMechanismException e) {
            alert=null;
            if (e.getCause() instanceof RecordStoreFullException)             alert=new Alert("Error","The mobile database is full",null,AlertType.ERROR);
 else             alert=new Alert("Error","The mobile database can not update new informations",null,AlertType.ERROR);
            Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
          }
          return true;
        }
        if (label.equals("View Favorites")) {
          _this.showMediaList(_this.getCurrentStoreName(),false,true);
          ScreenSingleton.getInstance().setCurrentScreenName(Constants.IMAGELIST_SCREEN);
          return true;
        }
        if (label.equals("Save")) {
          System.out.println("<* PhotoController.handleCommand() *> Save Photo Label = " + _this.screen.getLabelName());
          _this.getMedia().setMediaLabel(_this.screen.getLabelName());
          try {
            _this.updateMedia(_this.media);
          }
 catch (          InvalidImageDataException e) {
            alert=null;
            if (e instanceof ImagePathNotValidException)             alert=new Alert("Error","The path is not valid",null,AlertType.ERROR);
 else             alert=new Alert("Error","The image file format is not valid",null,AlertType.ERROR);
            Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
          }
catch (          PersistenceMechanismException e) {
            alert=new Alert("Error","The mobile database can not update this photo",null,AlertType.ERROR);
            Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
          }
          return _this.goToPreviousScreen();
        }
        if (label.equals("Back")) {
          return _this.goToPreviousScreen();
        }
        if (label.equals("Cancel")) {
          return _this.goToPreviousScreen();
        }
        return false;
      }
 catch (      ReturnBoolean r) {
        return r.value;
      }
    }
    protected MediaController _this;
    protected Command command;
    protected String label;
    protected AddMediaToAlbum form;
    protected String selectedImageName;
    protected String selectedMediaName;
    protected String selectedMediaName;
    protected CaptureVideoScreen playscree;
    protected VideoCaptureController controller;
    protected CaptureVideoScreen playscree;
    protected PhotoViewController controller;
    protected MediaData mymedia;
    protected Alert alert;
    protected Alert alert;
    protected Alert alert;
    protected String selectedMediaName;
    protected Alert alert;
    protected Alert alert;
    protected String selectedImageName;
    protected NewLabelScreen formScreen;
    protected Alert alert;
    protected String selectedMediaName;
    protected MediaData media;
    protected Alert alert;
    protected Alert alert;
    protected Alert alert;
    protected Alert alert;
    protected Alert alert;
    protected void hook21(){
    }
    protected void hook22(){
    }
    protected void hook23(){
    }
    protected void hook24() throws InvalidImageDataException, PersistenceMechanismException, ImageNotFoundException {
    }
    protected void hook25(){
    }
    protected void hook26(){
    }
    protected void hook27(){
    }
    protected void hook28(){
    }
  }
}
