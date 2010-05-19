package lancs.mobilemedia.core.ui.controller;
import javax.microedition.lcdui.Alert;
import javax.microedition.lcdui.AlertType;
import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.Display;
import javax.microedition.rms.RecordStoreFullException;
import lancs.mobilemedia.core.ui.MainUIMidlet;
import lancs.mobilemedia.core.ui.datamodel.AlbumData;
import lancs.mobilemedia.core.ui.datamodel.MediaData;
import lancs.mobilemedia.core.ui.screens.AddMediaToAlbum;
import lancs.mobilemedia.core.ui.screens.AlbumListScreen;
import lancs.mobilemedia.core.util.Constants;
import lancs.mobilemedia.lib.exceptions.ImageNotFoundException;
import lancs.mobilemedia.lib.exceptions.ImagePathNotValidException;
import lancs.mobilemedia.lib.exceptions.InvalidImageDataException;
import lancs.mobilemedia.lib.exceptions.PersistenceMechanismException;
import de.ovgu.cide.jakutil.*;
/** 
 * @author Eduardo Figueiredo
 * [EF] Added in Scenario 05
 */
public class PhotoViewController extends AbstractController {
  String imageName="";
  /** 
 * @param midlet
 * @param nextController
 * @param albumData
 * @param albumListScreen
 * @param currentScreenName
 */
  public PhotoViewController(  MainUIMidlet midlet,  AlbumData albumData,  AlbumListScreen albumListScreen,  String imageName){
    super(midlet,albumData,albumListScreen);
    this.imageName=imageName;
  }
  public boolean handleCommand(  Command c){
    return new PhotoViewController_handleCommand(this,c).execute();
  }
@MethodObject static class PhotoViewController_handleCommand {
    PhotoViewController_handleCommand(    PhotoViewController _this,    Command c){
      this._this=_this;
      this.c=c;
    }
    boolean execute(){
      try {
        label=c.getLabel();
        System.out.println("<* PhotoViewController.handleCommand() *> " + label);
        if (label.equals("Copy")) {
          copyPhotoToAlbum=new AddMediaToAlbum("Copy Photo to Album");
          copyPhotoToAlbum.setItemName(_this.imageName);
          copyPhotoToAlbum.setLabePath("Copy to Album:");
          copyPhotoToAlbum.setCommandListener(_this);
          this.hook37();
          Display.getDisplay(midlet).setCurrent(copyPhotoToAlbum);
          return true;
        }
 else         if (label.equals("Save Item")) {
          try {
            imageData=null;
            this.hook39();
            photoname=((AddMediaToAlbum)_this.getCurrentScreen()).getItemName();
            albumname=((AddMediaToAlbum)_this.getCurrentScreen()).getPath();
            this.hook38();
          }
 catch (          InvalidImageDataException e) {
            alert=null;
            if (e instanceof ImagePathNotValidException)             alert=new Alert("Error","The path is not valid",null,AlertType.ERROR);
 else             alert=new Alert("Error","The image file format is not valid",null,AlertType.ERROR);
            Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
            return true;
          }
catch (          PersistenceMechanismException e) {
            alert=null;
            if (e.getCause() instanceof RecordStoreFullException)             alert=new Alert("Error","The mobile database is full",null,AlertType.ERROR);
 else             alert=new Alert("Error","The mobile database can not add a new photo",null,AlertType.ERROR);
            Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
          }
          ((AlbumListScreen)_this.getAlbumListScreen()).repaintListAlbum(_this.getAlbumData().getAlbumNames());
          _this.setCurrentScreen(_this.getAlbumListScreen());
          ScreenSingleton.getInstance().setCurrentScreenName(Constants.ALBUMLIST_SCREEN);
          return true;
        }
        this.hook36();
        if ((label.equals("Cancel")) || (label.equals("Back"))) {
          ((AlbumListScreen)_this.getAlbumListScreen()).repaintListAlbum(_this.getAlbumData().getAlbumNames());
          _this.setCurrentScreen(_this.getAlbumListScreen());
          ScreenSingleton.getInstance().setCurrentScreenName(Constants.ALBUMLIST_SCREEN);
          return true;
        }
        return false;
      }
 catch (      ReturnBoolean r) {
        return r.value;
      }
    }
    protected PhotoViewController _this;
    protected Command c;
    protected String label;
    protected AddMediaToAlbum copyPhotoToAlbum;
    protected MediaData imageData;
    protected byte[] imgByte;
    protected Alert alert;
    protected String photoname;
    protected String albumname;
    protected Alert alert;
    protected Alert alert;
    protected byte[] newfoto;
    protected AddMediaToAlbum copyPhotoToAlbum;
    protected void hook36(){
    }
    protected void hook37(){
    }
    protected void hook38() throws InvalidImageDataException, PersistenceMechanismException {
      _this.getAlbumData().addMediaData(imageData,albumname);
    }
    protected void hook39() throws InvalidImageDataException, PersistenceMechanismException {
      try {
        imageData=_this.getAlbumData().getMediaInfo(_this.imageName);
      }
 catch (      ImageNotFoundException e) {
        alert=new Alert("Error","The selected photo was not found in the mobile device",null,AlertType.ERROR);
        Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
      }
    }
  }
}
