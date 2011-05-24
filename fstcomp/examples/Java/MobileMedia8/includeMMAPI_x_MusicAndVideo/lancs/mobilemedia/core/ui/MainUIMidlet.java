package lancs.mobilemedia.core.ui;
public class MainUIMidlet {
@MethodObject static class MainUIMidlet_startApp {
    protected void hook0() throws MIDletStateChangeException {
      selectcontroller2.setMusicAlbumData(_this.musicModel);
      selectcontroller2.setMusicController(_this.musicRootController);
      original();
    }
  }
}
