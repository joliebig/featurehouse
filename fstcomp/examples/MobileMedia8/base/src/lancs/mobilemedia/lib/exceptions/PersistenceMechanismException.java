package lancs.mobilemedia.lib.exceptions;
import de.ovgu.cide.jakutil.*;
public class PersistenceMechanismException extends Exception {
  public PersistenceMechanismException(  String arg0){
    super(arg0);
  }
  public PersistenceMechanismException(){
  }
  private Throwable cause;
  public PersistenceMechanismException(  Throwable arg0){
    cause=arg0;
  }
  public Throwable getCause(){
    return cause;
  }
}
