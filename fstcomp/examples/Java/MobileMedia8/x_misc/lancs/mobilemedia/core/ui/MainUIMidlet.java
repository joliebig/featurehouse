package lancs.mobilemedia.core.ui;
import lancs.mobilemedia.core.ui.controller.SelectMediaController;
import lancs.mobilemedia.core.ui.screens.SelectTypeOfMedia;
public class MainUIMidlet {
@MethodObject static class MainUIMidlet_startApp {
    protected void hook14() throws MIDletStateChangeException {
      Display.getDisplay(_this).setCurrent(mainscreen);
      ScreenSingleton.getInstance().setMainMenu(mainscreen);
      original();
    }
    protected void hook15() throws MIDletStateChangeException {
      mainscreen=new SelectTypeOfMedia();
      mainscreen.initMenu();
      original();
    }
  }
}
