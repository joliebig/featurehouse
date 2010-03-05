package lancs.mobilemedia.core.ui.datamodel;
public abstract class AlbumData {
  public void addPassword(  String albumname,  String passwd){
    mediaAccessor.addPassword(albumname,passwd);
  }
  public String getPassword(  String albumname){
    return mediaAccessor.getPassword(albumname);
  }
}
