package lancs.mobilemedia.core.ui.controller;
public class PhotoViewController {
@MethodObject static class PhotoViewController_handleCommand {
    protected void hook37(){
      if (((PhotoViewScreen)_this.getCurrentScreen()).isFromSMS()) {
        copyPhotoToAlbum.setCapturedMedia(((PhotoViewScreen)_this.getCurrentScreen()).getImage());
      }
      original();
    }
  }
}
