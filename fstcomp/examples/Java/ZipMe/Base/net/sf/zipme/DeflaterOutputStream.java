//

package net.sf.zipme;
import java.io.IOException;
import java.io.OutputStream;

/** 
 * This is a special OutputStream deflating the bytes that are
 * written through it.  It uses the Deflater for deflating.
 * A special thing to be noted is that flush() doesn't flush
 * everything in Sun's JDK, but it does so in jazzlib. This is because
 * Sun's Deflater doesn't have a way to flush() everything, without
 * finishing the stream.
 * @author Tom Tromey, Jochen Hoenicke
 * @date Jan 11, 2001 
 */
public class DeflaterOutputStream extends OutputStream {
  /** 
 * This is the subordinate <code>OutputStream</code> that this class
 * redirects its method calls to.
 */
  protected OutputStream out;
  /** 
 * This buffer is used temporarily to retrieve the bytes from the
 * deflater and write them to the underlying output stream.  
 */
  protected byte[] buf;
  /** 
 * The deflater which is used to deflate the stream.
 */
  protected Deflater def;
  /** 
 * Creates a new DeflaterOutputStream with a default Deflater and
 * default buffer size.
 * @param out the output stream where deflated output should be written.
 */
  public DeflaterOutputStream(  OutputStream out){
    this(out,new Deflater(),4096);
  }
  /** 
 * Creates a new DeflaterOutputStream with the given Deflater and
 * default buffer size.
 * @param out the output stream where deflated output should be written.
 * @param defl the underlying deflater.
 */
  public DeflaterOutputStream(  OutputStream out,  Deflater defl){
    this(out,defl,4096);
  }
  /** 
 * Creates a new DeflaterOutputStream with the given Deflater and
 * buffer size.
 * @param out the output stream where deflated output should be written.
 * @param defl the underlying deflater.
 * @param bufsize the buffer size.
 * @exception IllegalArgumentException if bufsize isn't positive.
 */
  public DeflaterOutputStream(  OutputStream out,  Deflater defl,  int bufsize){
    this.out=out;
    if (bufsize <= 0)     throw new IllegalArgumentException("bufsize <= 0");
    buf=new byte[bufsize];
    def=defl;
  }
}
