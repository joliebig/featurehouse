package lancs.mobilemedia.core.ui.controller;
public class MediaController {
@MethodObject static class MediaController_showImage {
    protected void hook20(){
      controller=new PhotoViewController(midlet,_this.getAlbumData(),(AlbumListScreen)_this.getAlbumListScreen(),name);
      controller.setNextController(nextcontroller);
      canv.setCommandListener(controller);
      nextcontroller=controller;
      original();
    }
  }
}
