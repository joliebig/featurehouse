package lancs.mobilemedia.core.ui.datamodel;
public class MediaData {
  public static String PHOTO="image/png";
  public static String MUSIC="audio/x-wav";
  public static String VIDEO="video/mpeg";
  private String typemedia;
  /** 
 * @return
 */
  public String getTypeMedia(){
    return typemedia;
  }
  /** 
 * @param type
 */
  public void setTypeMedia(  String type){
    this.typemedia=type;
  }
}
