package lancs.mobilemedia.core.ui;
import lancs.mobilemedia.core.ui.datamodel.MusicAlbumData;
public class MainUIMidlet {
  private BaseController musicRootController;
  private AlbumData musicModel;
@MethodObject static class MainUIMidlet_startApp {
    protected void hook9() throws MIDletStateChangeException {
      _this.musicModel=new MusicAlbumData();
      albumMusic=new AlbumListScreen();
      _this.musicRootController=new BaseController(_this,_this.musicModel,albumMusic);
      musicListController=new MediaListController(_this,_this.musicModel,albumMusic);
      musicListController.setNextController(_this.musicRootController);
      albumMusicController=new AlbumController(_this,_this.musicModel,albumMusic);
      albumMusicController.setNextController(musicListController);
      albumMusic.setCommandListener(albumMusicController);
      original();
    }
  }
}
