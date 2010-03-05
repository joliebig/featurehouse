package lancs.mobilemedia.core.ui;
public class MainUIMidlet {
@MethodObject static class MainUIMidlet_startApp {
    void execute() throws MIDletStateChangeException {
      original();
      _this.videoRootController.init(_this.videoModel);
    }
  }
}
