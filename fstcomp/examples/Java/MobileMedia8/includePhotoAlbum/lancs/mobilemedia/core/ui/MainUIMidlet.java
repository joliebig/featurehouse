package lancs.mobilemedia.core.ui;
import lancs.mobilemedia.core.ui.datamodel.ImageAlbumData;
public class MainUIMidlet {
  private BaseController imageRootController;
  private AlbumData imageModel;
@MethodObject static class MainUIMidlet_startApp {
    void execute() throws MIDletStateChangeException {
      _this.imageModel=new ImageAlbumData();
      album=new AlbumListScreen();
      _this.imageRootController=new BaseController(_this,_this.imageModel,album);
      photoListController=new MediaListController(_this,_this.imageModel,album);
      photoListController.setNextController(_this.imageRootController);
      albumController=new AlbumController(_this,_this.imageModel,album);
      albumController.setNextController(photoListController);
      album.setCommandListener(albumController);
      original();
    }
  }
}
