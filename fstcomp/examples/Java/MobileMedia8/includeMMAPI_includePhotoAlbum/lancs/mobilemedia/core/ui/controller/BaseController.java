package lancs.mobilemedia.core.ui.controller;
public class BaseController {
  protected void hook19(  String currentScreenName){
    if ((currentScreenName == null) || (currentScreenName.equals(Constants.ALBUMLIST_SCREEN))) {
      setCurrentScreen(ScreenSingleton.getInstance().getMainMenu());
      throw new ReturnBoolean(true);
    }
    original(currentScreenName);
  }
}
