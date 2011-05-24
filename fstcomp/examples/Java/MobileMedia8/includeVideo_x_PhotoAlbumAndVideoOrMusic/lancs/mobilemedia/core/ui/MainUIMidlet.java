package lancs.mobilemedia.core.ui;
public class MainUIMidlet {
@MethodObject static class MainUIMidlet_startApp {
    protected void hook4() throws MIDletStateChangeException {
      selectcontroller.setVideoAlbumData(_this.videoModel);
      selectcontroller.setVideoController(_this.videoRootController);
      original();
    }
  }
}
