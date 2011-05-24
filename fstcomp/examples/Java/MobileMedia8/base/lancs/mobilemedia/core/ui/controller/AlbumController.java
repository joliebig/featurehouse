package lancs.mobilemedia.core.ui.controller;
import javax.microedition.lcdui.Alert;
import javax.microedition.lcdui.AlertType;
import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.Display;
import javax.microedition.lcdui.List;
import javax.microedition.rms.RecordStoreFullException;
import lancs.mobilemedia.core.ui.MainUIMidlet;
import lancs.mobilemedia.core.ui.datamodel.AlbumData;
import lancs.mobilemedia.core.ui.screens.AlbumListScreen;
import lancs.mobilemedia.core.ui.screens.NewLabelScreen;
import lancs.mobilemedia.core.util.Constants;
import lancs.mobilemedia.lib.exceptions.InvalidPhotoAlbumNameException;
import lancs.mobilemedia.lib.exceptions.PersistenceMechanismException;
import de.ovgu.cide.jakutil.*;
/** 
 * @author Eduardo Figueiredo
 * Added in the Scenario 04.
 * Purpose: simplify method handleCommand() in the BaseController.
 */
public class AlbumController extends AbstractController {
  public AlbumController(  MainUIMidlet midlet,  AlbumData albumData,  AlbumListScreen albumListScreen){
    super(midlet,albumData,albumListScreen);
  }
  public boolean handleCommand(  Command command){
    return new AlbumController_handleCommand(this,command).execute();
  }
  /** 
 * This option is mainly for testing purposes. If the record store
 * on the device or emulator gets into an unstable state, or has too 
 * many images, you can reset it, which clears the record stores and
 * re-creates them with the default images bundled with the application 
 */
  private void resetMediaData(){
    try {
      getAlbumData().resetMediaData();
    }
 catch (    PersistenceMechanismException e) {
      Alert alert=null;
      if (e.getCause() instanceof RecordStoreFullException)       alert=new Alert("Error","The mobile database is full",null,AlertType.ERROR);
 else       alert=new Alert("Error","It is not possible to reset the database",null,AlertType.ERROR);
      Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
      return;
    }
    for (int i=0; i < getAlbumListScreen().size(); i++) {
      getAlbumListScreen().delete(i);
    }
    String[] albumNames=getAlbumData().getAlbumNames();
    for (int i=0; i < albumNames.length; i++) {
      if (albumNames[i] != null) {
        getAlbumListScreen().append(albumNames[i],null);
      }
    }
    setCurrentScreen(getAlbumListScreen());
  }
  private void goToPreviousScreen(){
    System.out.println("<* AlbumController.goToPreviousScreen() *>");
    ((AlbumListScreen)getAlbumListScreen()).repaintListAlbum(getAlbumData().getAlbumNames());
    setCurrentScreen(getAlbumListScreen());
    ScreenSingleton.getInstance().setCurrentScreenName(Constants.ALBUMLIST_SCREEN);
  }
@MethodObject static class AlbumController_handleCommand {
    AlbumController_handleCommand(    AlbumController _this,    Command command){
      this._this=_this;
      this.command=command;
    }
    boolean execute(){
      try {
        label=command.getLabel();
        System.out.println("<* AlbumController.handleCommand() *>: " + label);
        if (label.equals("Reset")) {
          System.out.println("<* BaseController.handleCommand() *> Reset Photo Album");
          _this.resetMediaData();
          ScreenSingleton.getInstance().setCurrentScreenName(Constants.ALBUMLIST_SCREEN);
          return true;
        }
 else         if (label.equals("New Album")) {
          System.out.println("Create new Photo Album here");
          ScreenSingleton.getInstance().setCurrentScreenName(Constants.NEWALBUM_SCREEN);
          canv=new NewLabelScreen("Add new Photo Album",NewLabelScreen.NEW_ALBUM);
          canv.setCommandListener(_this);
          _this.setCurrentScreen(canv);
          canv=null;
          return true;
        }
 else         if (label.equals("Delete Album")) {
          System.out.println("Delete Photo Album here");
          down=(List)Display.getDisplay(midlet).getCurrent();
          ScreenSingleton.getInstance().setCurrentScreenName(Constants.CONFIRMDELETEALBUM_SCREEN);
          ScreenSingleton.getInstance().setCurrentStoreName(down.getString(down.getSelectedIndex()));
          message="Would you like to remove the album " + ScreenSingleton.getInstance().getCurrentStoreName();
          deleteConfAlert=new Alert("Delete Photo Album",message,null,AlertType.CONFIRMATION);
          deleteConfAlert.setTimeout(Alert.FOREVER);
          deleteConfAlert.addCommand(new Command("Yes - Delete",Command.OK,2));
          deleteConfAlert.addCommand(new Command("No - Delete",Command.CANCEL,2));
          _this.setAlbumListAsCurrentScreen(deleteConfAlert);
          deleteConfAlert.setCommandListener(_this);
          return true;
        }
 else         if (label.equals("Yes - Delete")) {
{
          }
          this.hook17();
          try {
            _this.getAlbumData().deleteAlbum(ScreenSingleton.getInstance().getCurrentStoreName());
          }
 catch (          PersistenceMechanismException e) {
            alert=new Alert("Error","The mobile database can not delete this photo album",null,AlertType.ERROR);
            Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
          }
          _this.goToPreviousScreen();
          return true;
        }
 else         if (label.equals("No - Delete")) {
          _this.goToPreviousScreen();
          return true;
        }
 else         if (label.equals("Save")) {
          try {
            if (_this.getCurrentScreen() instanceof NewLabelScreen) {
              currentScreen=(NewLabelScreen)_this.getCurrentScreen();
              if (currentScreen.getFormType() == NewLabelScreen.NEW_ALBUM) {
                _this.getAlbumData().createNewAlbum(currentScreen.getLabelName());
              }
 else               if (currentScreen.getFormType() == NewLabelScreen.LABEL_PHOTO) {
              }
            }
            this.hook18();
          }
 catch (          PersistenceMechanismException e) {
            alert=null;
            if (e.getCause() instanceof RecordStoreFullException)             alert=new Alert("Error","The mobile database is full",null,AlertType.ERROR);
 else             alert=new Alert("Error","The mobile database can not add a new photo album",null,AlertType.ERROR);
            Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
            return true;
          }
catch (          InvalidPhotoAlbumNameException e) {
            alert=new Alert("Error","You have provided an invalid Photo Album name",null,AlertType.ERROR);
            Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
            return true;
          }
          _this.goToPreviousScreen();
          return true;
        }
        this.hook16();
        return false;
      }
 catch (      ReturnBoolean r) {
        return r.value;
      }
    }
    protected AlbumController _this;
    protected Command command;
    protected String label;
    protected NewLabelScreen canv;
    protected List down;
    protected String message;
    protected Alert deleteConfAlert;
    protected String passwd;
    protected PasswordScreen pwd;
    protected Alert alert;
    protected Alert alert;
    protected NewLabelScreen currentScreen;
    protected Alert alert;
    protected Alert alert;
    protected Alert alert;
    protected Alert alert;
    protected String message;
    protected Alert definePassword;
    protected PasswordScreen pwd;
    protected Alert alert;
    protected Alert alert;
    protected PasswordScreen password;
    protected String passwd;
    protected Alert alert;
    protected Alert alert;
    protected PasswordScreen pwd;
    protected PasswordScreen password;
    protected void hook16(){
    }
    protected void hook17(){
    }
    protected void hook18() throws PersistenceMechanismException, InvalidPhotoAlbumNameException {
    }
  }
}
