package lancs.mobilemedia.core.ui.controller;
import lancs.mobilemedia.core.ui.screens.PasswordScreen;
public class AlbumController {
  private NewLabelScreen albumName=null;
  private PasswordScreen password=null;
  private String albumtodelete=" ";
@MethodObject static class AlbumController_handleCommand {
    protected void hook16(){
      if (label.equals("OK")) {
        _this.albumName=(NewLabelScreen)_this.getCurrentScreen();
        message="Would you like to define a password to this Album";
        definePassword=new Alert("Define a Password",message,null,AlertType.CONFIRMATION);
        definePassword.setTimeout(Alert.FOREVER);
        definePassword.addCommand(new Command("Yes",Command.OK,2));
        definePassword.addCommand(new Command("No",Command.EXIT,2));
        _this.setAlbumListAsCurrentScreen(definePassword);
        definePassword.setCommandListener(_this);
        throw new ReturnBoolean(true);
      }
 else       if (label.equals("Yes")) {
        pwd=new PasswordScreen("Define a Password",0);
        pwd.setCommandListener(_this);
        _this.setCurrentScreen(pwd);
        pwd=null;
        throw new ReturnBoolean(true);
      }
 else       if (label.equals("No")) {
        try {
          _this.getAlbumData().createNewAlbum(_this.albumName.getLabelName());
        }
 catch (        PersistenceMechanismException e) {
          alert=null;
          if (e.getCause() instanceof RecordStoreFullException)           alert=new Alert("Error","The mobile database is full",null,AlertType.ERROR);
 else           alert=new Alert("Error","The mobile database can not add a new photo album",null,AlertType.ERROR);
          Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
          throw new ReturnBoolean(true);
        }
catch (        InvalidPhotoAlbumNameException e) {
          alert=new Alert("Error","You have provided an invalid Photo Album name",null,AlertType.ERROR);
          Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
          throw new ReturnBoolean(true);
        }
        _this.goToPreviousScreen();
        throw new ReturnBoolean(true);
      }
 else       if (label.equals("Confirm")) {
        System.out.println("<* AlbumController.handleCommand() *>: " + label);
        password=(PasswordScreen)_this.getCurrentScreen();
        passwd=_this.getAlbumData().getPassword(_this.getCurrentStoreName());
        if (password.getPassword().equals(passwd)) {
          try {
            _this.getAlbumData().deleteAlbum(ScreenSingleton.getInstance().getCurrentStoreName());
          }
 catch (          PersistenceMechanismException e) {
            System.out.println(e);
            alert=new Alert("Error","The mobile database can not delete this photo album",null,AlertType.ERROR);
            Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
          }
          _this.goToPreviousScreen();
        }
 else {
          alert=new Alert("Error","Invalid Password",null,AlertType.ERROR);
          Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
        }
        throw new ReturnBoolean(true);
      }
 else       if (label.equals("Add Password")) {
        pwd=new PasswordScreen("Define a Password",3);
        pwd.setCommandListener(_this);
        _this.setCurrentScreen(pwd);
        pwd=null;
        throw new ReturnBoolean(true);
      }
 else       if (label.equals("Store")) {
        password=(PasswordScreen)_this.getCurrentScreen();
        _this.getAlbumData().addPassword(ScreenSingleton.getInstance().getCurrentStoreName(),password.getPassword());
        _this.goToPreviousScreen();
        throw new ReturnBoolean(true);
      }
      original();
    }
    protected void hook17(){
      _this.albumtodelete=ScreenSingleton.getInstance().getCurrentStoreName();
      passwd=_this.getAlbumData().getPassword(_this.albumtodelete);
      if (passwd != null) {
        pwd=new PasswordScreen("Password",1);
        pwd.setCommandListener(_this);
        _this.setCurrentScreen(pwd);
        pwd=null;
      }
 else {
        try {
          _this.getAlbumData().deleteAlbum(ScreenSingleton.getInstance().getCurrentStoreName());
        }
 catch (        PersistenceMechanismException e) {
          alert=new Alert("Error","The mobile database can not delete this photo album",null,AlertType.ERROR);
          Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
        }
        _this.goToPreviousScreen();
      }
      original();
    }
    protected void hook18() throws PersistenceMechanismException, InvalidPhotoAlbumNameException {
      try {
        _this.password=(PasswordScreen)_this.getCurrentScreen();
        _this.getAlbumData().createNewAlbum(_this.albumName.getLabelName());
        _this.getAlbumData().addPassword(_this.albumName.getLabelName(),_this.password.getPassword());
      }
 catch (      PersistenceMechanismException e) {
        alert=null;
        if (e.getCause() instanceof RecordStoreFullException)         alert=new Alert("Error","The mobile database is full",null,AlertType.ERROR);
 else         alert=new Alert("Error","The mobile database can not add a new photo album",null,AlertType.ERROR);
        Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
        throw new ReturnBoolean(true);
      }
catch (      InvalidPhotoAlbumNameException e) {
        alert=new Alert("Error","You have provided an invalid Photo Album name",null,AlertType.ERROR);
        Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
        throw new ReturnBoolean(true);
      }
      original();
    }
  }
}
