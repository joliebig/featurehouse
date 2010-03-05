package lancs.mobilemedia.lib.exceptions;
import de.ovgu.cide.jakutil.*;
public class InvalidImageFormatException extends InvalidImageDataException {
  public InvalidImageFormatException(){
  }
  public InvalidImageFormatException(  String arg0){
    super(arg0);
  }
  public InvalidImageFormatException(  Throwable arg0){
    super(arg0);
  }
}
