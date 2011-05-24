package lancs.mobilemedia.core.ui.controller;
import javax.microedition.lcdui.Alert;
import javax.microedition.lcdui.AlertType;
import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.Display;
import javax.microedition.lcdui.List;
import lancs.mobilemedia.lib.exceptions.UnavailablePhotoAlbumException;
import lancs.mobilemedia.core.ui.MainUIMidlet;
import lancs.mobilemedia.core.ui.datamodel.AlbumData;
import lancs.mobilemedia.core.ui.datamodel.MediaData;
import lancs.mobilemedia.core.ui.screens.AlbumListScreen;
import lancs.mobilemedia.core.ui.screens.MediaListScreen;
import lancs.mobilemedia.core.util.Constants;
import de.ovgu.cide.jakutil.*;
/** 
 * @author Eduardo Figueiredo
 */
public class MediaListController extends AbstractController {
  /** 
 * @param midlet
 * @param nextController
 * @param albumData
 * @param albumListScreen
 */
  public MediaListController(  MainUIMidlet midlet,  AlbumData albumData,  AlbumListScreen albumListScreen){
    super(midlet,albumData,albumListScreen);
  }
  public boolean handleCommand(  Command command){
    return new MediaListController_handleCommand(this,command).execute();
  }
  /** 
 * Show the list of images in the record store
 * TODO: Refactor - Move this to ImageList class
 */
  public void showMediaList(  String recordName,  boolean sort,  boolean favorite){
    if (recordName == null)     recordName=getCurrentStoreName();
    MediaController mediaController=new MediaController(midlet,getAlbumData(),(AlbumListScreen)getAlbumListScreen());
    mediaController.setNextController(this);
    MediaListScreen mediaList=null;
    mediaList=this.hook30(mediaList);
    mediaList=this.hook29(mediaList);
    mediaList=this.hook34(mediaList);
    mediaList.setCommandListener(mediaController);
    mediaList.initMenu();
    MediaData[] medias=null;
    try {
      medias=getAlbumData().getMedias(recordName);
    }
 catch (    UnavailablePhotoAlbumException e) {
      Alert alert=new Alert("Error","The list of items can not be recovered",null,AlertType.ERROR);
      Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
      return;
    }
    if (medias == null)     return;
    this.hook33(sort,medias);
    for (int i=0; i < medias.length; i++) {
      if (medias[i] != null) {
        if (!favorite)         mediaList.append(medias[i].getMediaLabel(),null);
 else         if (medias[i].isFavorite())         mediaList.append(medias[i].getMediaLabel(),null);
      }
    }
    setCurrentScreen(mediaList);
  }
@MethodObject static class MediaListController_handleCommand {
    MediaListController_handleCommand(    MediaListController _this,    Command command){
      this._this=_this;
      this.command=command;
    }
    boolean execute(){
      try {
        label=command.getLabel();
{
        }
        if (label.equals("Select")) {
          down=(List)Display.getDisplay(midlet).getCurrent();
          ScreenSingleton.getInstance().setCurrentStoreName(down.getString(down.getSelectedIndex()));
          this.hook32();
          _this.showMediaList(_this.getCurrentStoreName(),false,false);
          ScreenSingleton.getInstance().setCurrentScreenName(Constants.IMAGELIST_SCREEN);
          return true;
        }
        this.hook31();
        return false;
      }
 catch (      ReturnBoolean r) {
        return r.value;
      }
    }
    protected MediaListController _this;
    protected Command command;
    protected String label;
    protected String passwd;
    protected String ps2;
    protected List down;
    protected PasswordScreen pwd;
    protected PasswordScreen password;
    protected Alert alert;
    protected void hook31(){
    }
    protected void hook32(){
    }
  }
  protected MediaListScreen hook29(  MediaListScreen mediaList){
    return mediaList;
  }
  protected MediaListScreen hook30(  MediaListScreen mediaList){
    return mediaList;
  }
  protected void hook33(  boolean sort,  MediaData[] medias){
  }
  protected MediaListScreen hook34(  MediaListScreen mediaList){
    return mediaList;
  }
}
