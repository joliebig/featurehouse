package lancs.mobilemedia.core.ui;
public class MainUIMidlet {
@MethodObject static class MainUIMidlet_startApp {
    protected void hook8() throws MIDletStateChangeException {
      _this.imageRootController.init(_this.imageModel);
      original();
    }
  }
}
