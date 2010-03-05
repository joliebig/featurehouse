package lancs.mobilemedia.core.ui.controller;
import lancs.mobilemedia.core.ui.datamodel.ImageAlbumData;
import javax.microedition.lcdui.Image;
import lancs.mobilemedia.core.ui.screens.PhotoViewScreen;
public class MediaController {
  /** 
 * Show the current image that is selected
 */
  public void showImage(  String name){
    new MediaController_showImage(this,name).execute();
  }
@MethodObject static class MediaController_showImage {
    MediaController_showImage(    MediaController _this,    String name){
      this._this=_this;
      this.name=name;
    }
    void execute(){
      storedImage=null;
      try {
        storedImage=((ImageAlbumData)_this.getAlbumData()).getImageFromRecordStore(_this.getCurrentStoreName(),name);
      }
 catch (      ImageNotFoundException e) {
        alert=new Alert("Error","The selected photo was not found in the mobile device",null,AlertType.ERROR);
        Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
        return;
      }
catch (      PersistenceMechanismException e) {
        alert=new Alert("Error","The mobile database can open this photo",null,AlertType.ERROR);
        Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
        return;
      }
      canv=new PhotoViewScreen(storedImage);
      canv.setCommandListener(_this);
      nextcontroller=_this;
      this.hook20();
      smscontroller=new SmsSenderController(midlet,_this.getAlbumData(),(AlbumListScreen)_this.getAlbumListScreen(),name);
      smscontroller.setNextController(nextcontroller);
      canv.setCommandListener(smscontroller);
      nextcontroller=smscontroller;
      _this.setCurrentScreen(canv);
    }
    protected MediaController _this;
    protected String name;
    protected Image storedImage;
    protected Alert alert;
    protected Alert alert;
    protected PhotoViewScreen canv;
    protected AbstractController nextcontroller;
    protected PhotoViewController controller;
    protected SmsSenderController smscontroller;
    protected void hook20(){
    }
  }
@MethodObject static class MediaController_handleCommand {
    protected void hook26(){
      if (label.equals("View")) {
        selectedImageName=_this.getSelectedMediaName();
        _this.showImage(selectedImageName);
        _this.incrementCountViews(selectedImageName);
        ScreenSingleton.getInstance().setCurrentScreenName(Constants.IMAGE_SCREEN);
        throw new ReturnBoolean(true);
      }
      original();
    }
  }
}
