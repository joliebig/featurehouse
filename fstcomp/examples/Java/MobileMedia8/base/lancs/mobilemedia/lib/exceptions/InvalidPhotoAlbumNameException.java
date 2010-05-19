package lancs.mobilemedia.lib.exceptions;
import de.ovgu.cide.jakutil.*;
public class InvalidPhotoAlbumNameException extends Exception {
  public InvalidPhotoAlbumNameException(){
  }
  public InvalidPhotoAlbumNameException(  String s){
    super(s);
  }
}
