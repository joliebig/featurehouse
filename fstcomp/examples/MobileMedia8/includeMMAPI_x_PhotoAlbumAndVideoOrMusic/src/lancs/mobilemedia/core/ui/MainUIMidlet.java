package lancs.mobilemedia.core.ui;
public class MainUIMidlet {
@MethodObject static class MainUIMidlet_startApp {
    protected void hook2() throws MIDletStateChangeException {
      selectcontroller.setMusicAlbumData(_this.musicModel);
      selectcontroller.setMusicController(_this.musicRootController);
      original();
    }
  }
}
