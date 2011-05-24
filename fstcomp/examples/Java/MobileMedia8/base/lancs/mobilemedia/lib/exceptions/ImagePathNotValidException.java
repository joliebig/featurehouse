package lancs.mobilemedia.lib.exceptions;
import de.ovgu.cide.jakutil.*;
public class ImagePathNotValidException extends InvalidImageDataException {
  private Throwable cause;
  public ImagePathNotValidException(){
  }
  public ImagePathNotValidException(  String arg0){
    super(arg0);
  }
  public ImagePathNotValidException(  Throwable arg0){
    cause=arg0;
  }
  public Throwable getCause(){
    return cause;
  }
}
