package lancs.mobilemedia.core.ui.controller;
public class PhotoViewController {
  CaptureVideoScreen cpVideoScreen=null;
  public CaptureVideoScreen getCpVideoScreen(){
    return cpVideoScreen;
  }
  public void setCpVideoScreen(  CaptureVideoScreen cpVideoScreen){
    this.cpVideoScreen=cpVideoScreen;
  }
@MethodObject static class PhotoViewController_handleCommand {
    protected void hook36(){
      if (label.equals("Take photo")) {
        System.out.println("Olha para a captura" + _this.cpVideoScreen);
        newfoto=_this.cpVideoScreen.takePicture();
        System.out.println("Obteve a imagem");
        copyPhotoToAlbum=new AddMediaToAlbum("Copy Photo to Album");
        System.out.println("Crio a screen");
        copyPhotoToAlbum.setItemName("New picture");
        copyPhotoToAlbum.setLabePath("Copy to Album:");
        copyPhotoToAlbum.setCommandListener(_this);
        copyPhotoToAlbum.setCapturedMedia(newfoto);
        System.out.println("Definiu a imagem");
        Display.getDisplay(midlet).setCurrent(copyPhotoToAlbum);
        throw new ReturnBoolean(true);
      }
      original();
    }
  }
}
