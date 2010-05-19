package lancs.mobilemedia.core.ui.controller;
public class SelectMediaController {
  BaseController imageController;
  AlbumData imageAlbumData;
  public BaseController getImageController(){
    return imageController;
  }
  public void setImageController(  BaseController imageController){
    this.imageController=imageController;
  }
  public AlbumData getImageAlbumData(){
    return imageAlbumData;
  }
  public void setImageAlbumData(  AlbumData imageAlbumData){
    this.imageAlbumData=imageAlbumData;
  }
  protected void hook42(  List down){
    if (down.getString(down.getSelectedIndex()).equals("Photos"))     imageController.init(imageAlbumData);
    original(down);
  }
}
