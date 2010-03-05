package lancs.mobilemedia.lib.exceptions;
import de.ovgu.cide.jakutil.*;
public class UnavailablePhotoAlbumException extends Exception {
  private Throwable cause;
  public UnavailablePhotoAlbumException(){
  }
  public UnavailablePhotoAlbumException(  String arg0){
    super(arg0);
  }
  public UnavailablePhotoAlbumException(  Throwable arg0){
    cause=arg0;
  }
  public Throwable getCause(){
    return cause;
  }
}
