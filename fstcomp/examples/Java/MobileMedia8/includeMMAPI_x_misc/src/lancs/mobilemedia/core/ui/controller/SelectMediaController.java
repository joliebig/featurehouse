package lancs.mobilemedia.core.ui.controller;
public class SelectMediaController {
  BaseController musicController;
  AlbumData musicAlbumData;
  public BaseController getMusicController(){
    return musicController;
  }
  public void setMusicController(  BaseController musicController){
    this.musicController=musicController;
  }
  public AlbumData getMusicAlbumData(){
    return musicAlbumData;
  }
  public void setMusicAlbumData(  AlbumData musicAlbumData){
    this.musicAlbumData=musicAlbumData;
  }
  protected void hook41(  List down){
    if (down.getString(down.getSelectedIndex()).equals("Music"))     musicController.init(musicAlbumData);
    original(down);
  }
}
