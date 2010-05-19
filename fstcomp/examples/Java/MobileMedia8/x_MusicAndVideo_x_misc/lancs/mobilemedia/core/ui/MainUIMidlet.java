package lancs.mobilemedia.core.ui;
public class MainUIMidlet {
@MethodObject static class MainUIMidlet_startApp {
    protected void hook5() throws MIDletStateChangeException {
      mainscreen.setCommandListener(selectcontroller2);
      original();
    }
  }
}
