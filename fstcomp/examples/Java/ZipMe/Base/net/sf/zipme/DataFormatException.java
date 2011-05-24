//

package net.sf.zipme;

/** 
 * Exception thrown when compressed data is corrupt.
 * @author Tom Tromey
 * @author John Leuner
 * @since 1.1
 * @status updated to 1.4
 */
public class DataFormatException extends Exception {
  /** 
 * Compatible with JDK 1.1+.
 */
  private static final long serialVersionUID=2219632870893641452L;
  /** 
 * Create an exception without a message.
 */
  public DataFormatException(){
  }
  /** 
 * Create an exception with a message.
 * @param msg the message
 */
  public DataFormatException(  String msg){
    super(msg);
  }
}
