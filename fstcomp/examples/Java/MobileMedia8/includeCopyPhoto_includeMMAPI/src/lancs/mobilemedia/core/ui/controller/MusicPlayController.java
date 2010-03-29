package lancs.mobilemedia.core.ui.controller;
public class MusicPlayController {
  private String mediaName;
  public String getMediaName(){
    return mediaName;
  }
  public void setMediaName(  String mediaName){
    this.mediaName=mediaName;
  }
@MethodObject static class MusicPlayController_handleCommand {
    protected void hook35(){
      if (label.equals("Copy")) {
        copyPhotoToAlbum=new AddMediaToAlbum("Copy Media to Album");
        copyPhotoToAlbum.setItemName(_this.mediaName);
        copyPhotoToAlbum.setLabePath("Copy to Album:");
        copyPhotoToAlbum.setCommandListener(_this);
        Display.getDisplay(midlet).setCurrent(copyPhotoToAlbum);
        throw new ReturnBoolean(true);
      }
      if (label.equals("Save Item")) {
        try {
          imageData=null;
          try {
            imageData=_this.getAlbumData().getMediaInfo(_this.mediaName);
          }
 catch (          ImageNotFoundException e) {
            alert=new Alert("Error","The selected photo was not found in the mobile device",null,AlertType.ERROR);
            Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
          }
          albumname=((AddMediaToAlbum)_this.getCurrentScreen()).getPath();
          _this.getAlbumData().addMediaData(imageData,albumname);
        }
 catch (        InvalidImageDataException e) {
          alert=null;
          if (e instanceof ImagePathNotValidException)           alert=new Alert("Error","The path is not valid",null,AlertType.ERROR);
 else           alert=new Alert("Error","The music file format is not valid",null,AlertType.ERROR);
          Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
          throw new ReturnBoolean(true);
        }
catch (        PersistenceMechanismException e) {
          alert=null;
          if (e.getCause() instanceof RecordStoreFullException)           alert=new Alert("Error","The mobile database is full",null,AlertType.ERROR);
 else           alert=new Alert("Error","The mobile database can not add a new music",null,AlertType.ERROR);
          Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
        }
        ((AlbumListScreen)_this.getAlbumListScreen()).repaintListAlbum(_this.getAlbumData().getAlbumNames());
        _this.setCurrentScreen(_this.getAlbumListScreen());
        ScreenSingleton.getInstance().setCurrentScreenName(Constants.ALBUMLIST_SCREEN);
        throw new ReturnBoolean(true);
      }
      original();
    }
  }
}
