package lancs.mobilemedia.core.ui;
import lancs.mobilemedia.core.ui.datamodel.VideoAlbumData;
public class MainUIMidlet {
  private BaseController videoRootController;
  private AlbumData videoModel;
@MethodObject static class MainUIMidlet_startApp {
    protected void hook11() throws MIDletStateChangeException {
      _this.videoModel=new VideoAlbumData();
      albumVideo=new AlbumListScreen();
      _this.videoRootController=new BaseController(_this,_this.videoModel,albumVideo);
      videoListController=new MediaListController(_this,_this.videoModel,albumVideo);
      videoListController.setNextController(_this.videoRootController);
      albumVideoController=new AlbumController(_this,_this.videoModel,albumVideo);
      albumVideoController.setNextController(videoListController);
      albumVideo.setCommandListener(albumVideoController);
      original();
    }
  }
}
