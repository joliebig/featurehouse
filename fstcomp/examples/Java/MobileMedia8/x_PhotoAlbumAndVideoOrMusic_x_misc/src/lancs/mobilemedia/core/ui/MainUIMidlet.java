package lancs.mobilemedia.core.ui;
public class MainUIMidlet {
@MethodObject static class MainUIMidlet_startApp {
    protected void hook6() throws MIDletStateChangeException {
      mainscreen.setCommandListener(selectcontroller);
      original();
    }
  }
}
