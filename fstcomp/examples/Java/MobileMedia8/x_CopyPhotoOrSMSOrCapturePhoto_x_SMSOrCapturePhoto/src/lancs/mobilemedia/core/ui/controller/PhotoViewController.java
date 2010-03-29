package lancs.mobilemedia.core.ui.controller;
public class PhotoViewController {
@MethodObject static class PhotoViewController_handleCommand {
    protected void hook38() throws InvalidImageDataException, PersistenceMechanismException {
      if (imgByte.length > 0)       _this.getAlbumData().addImageData(photoname,imgByte,albumname);
      if (imgByte.length == 0)       original();
    }
    protected void hook39() throws InvalidImageDataException, PersistenceMechanismException {
      imgByte=((AddMediaToAlbum)_this.getCurrentScreen()).getCapturedMedia();
      if (imgByte.length == 0) {
        original();
      }
    }
  }
}
