package lancs.mobilemedia.core.ui.controller;
import lancs.mobilemedia.core.ui.screens.PasswordScreen;
public class MediaListController {
@MethodObject static class MediaListController_handleCommand {
    protected void hook31(){
      if (label.equals("Confirm")) {
        password=(PasswordScreen)_this.getCurrentScreen();
        passwd=_this.getAlbumData().getPassword(_this.getCurrentStoreName());
        if (password.getPassword().equals(passwd)) {
          _this.showMediaList(_this.getCurrentStoreName(),false,false);
          ScreenSingleton.getInstance().setCurrentScreenName(Constants.IMAGELIST_SCREEN);
        }
 else {
          alert=new Alert("Error","Invalid Password",null,AlertType.ERROR);
          Display.getDisplay(midlet).setCurrent(alert,Display.getDisplay(midlet).getCurrent());
        }
        throw new ReturnBoolean(true);
      }
      original();
    }
    protected void hook32(){
      passwd=ScreenSingleton.getInstance().getCurrentStoreName();
      ps2=_this.getAlbumData().getPassword(passwd);
      if (ps2 == null) {
        _this.showMediaList(ScreenSingleton.getInstance().getCurrentStoreName(),false,false);
        ScreenSingleton.getInstance().setCurrentScreenName(Constants.IMAGELIST_SCREEN);
      }
 else {
        pwd=new PasswordScreen("Password",1);
        pwd.setCommandListener(_this);
        _this.setCurrentScreen(pwd);
        pwd=null;
        throw new ReturnBoolean(true);
      }
      original();
    }
  }
}
