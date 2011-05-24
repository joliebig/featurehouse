package lancs.mobilemedia.core.ui;
public class MainUIMidlet {
@MethodObject static class MainUIMidlet_startApp {
    protected void hook7() throws MIDletStateChangeException {
      _this.musicRootController.init(_this.musicModel);
      original();
    }
  }
}
