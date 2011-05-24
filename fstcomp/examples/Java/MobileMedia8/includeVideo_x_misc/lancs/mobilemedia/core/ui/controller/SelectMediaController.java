package lancs.mobilemedia.core.ui.controller;
public class SelectMediaController {
  BaseController videoController;
  AlbumData videoAlbumData;
  public BaseController getVideoController(){
    return videoController;
  }
  public void setVideoController(  BaseController videoController){
    this.videoController=videoController;
  }
  public AlbumData getVideoAlbumData(){
    return videoAlbumData;
  }
  public void setVideoAlbumData(  AlbumData videoAlbumData){
    this.videoAlbumData=videoAlbumData;
  }
  protected void hook43(  List down){
    if (down.getString(down.getSelectedIndex()).equals("Videos"))     videoController.init(videoAlbumData);
    original(down);
  }
}
