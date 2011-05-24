package lancs.mobilemedia.core.ui.controller;
import javax.microedition.lcdui.Command;
import lancs.mobilemedia.core.ui.MainUIMidlet;
import lancs.mobilemedia.core.ui.datamodel.AlbumData;
import lancs.mobilemedia.core.ui.screens.AlbumListScreen;
import lancs.mobilemedia.core.util.Constants;
import de.ovgu.cide.jakutil.*;
/** 
 * @author tyoung
 * This is the base controller class used in the MVC architecture.
 * It controls the flow of screens for the MobilePhoto application.
 * Commands handled by this class should only be for the core application
 * that runs on any MIDP platform. Each device or class of devices that supports
 * optional features will extend this class to handle feature specific commands.
 */
public class BaseController extends AbstractController {
  /** 
 * Pass a handle to the main Midlet for this controller
 * @param midlet
 */
  public BaseController(  MainUIMidlet midlet,  AlbumData model,  AlbumListScreen albumListScreen){
    super(midlet,model,albumListScreen);
  }
  /** 
 * Initialize the controller
 */
  public void init(  AlbumData model){
    String[] albumNames=model.getAlbumNames();
    getAlbumListScreen().deleteAll();
    for (int i=0; i < albumNames.length; i++) {
      if (albumNames[i] != null) {
        getAlbumListScreen().append(albumNames[i],null);
      }
    }
    ((AlbumListScreen)getAlbumListScreen()).initMenu();
    setCurrentScreen(getAlbumListScreen());
  }
  public boolean handleCommand(  Command command){
    String label=command.getLabel();
    System.out.println(this.getClass().getName() + "::handleCommand: " + label);
    if (label.equals("Exit")) {
      midlet.destroyApp(true);
      return true;
    }
 else     if (label.equals("Back")) {
      return goToPreviousScreen();
    }
 else     if (label.equals("Cancel")) {
      return goToPreviousScreen();
    }
    return false;
  }
  private boolean goToPreviousScreen(){
    try {
      String currentScreenName=ScreenSingleton.getInstance().getCurrentScreenName();
      System.out.println("<* BaseController.goToPreviousScreen() **>" + currentScreenName);
      if (currentScreenName != null) {
        if ((currentScreenName.equals(Constants.IMAGELIST_SCREEN)) || (currentScreenName.equals(Constants.NEWALBUM_SCREEN)) || (currentScreenName.equals(Constants.CONFIRMDELETEALBUM_SCREEN))) {
          ((AlbumListScreen)getAlbumListScreen()).repaintListAlbum(getAlbumData().getAlbumNames());
          setCurrentScreen(getAlbumListScreen());
          ScreenSingleton.getInstance().setCurrentScreenName(Constants.ALBUMLIST_SCREEN);
          return true;
        }
      }
      this.hook19(currentScreenName);
      return false;
    }
 catch (    ReturnBoolean r) {
      return r.value;
    }
  }
  protected void hook19(  String currentScreenName){
  }
}
