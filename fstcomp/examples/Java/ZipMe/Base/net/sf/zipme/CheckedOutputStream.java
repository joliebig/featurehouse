//

package net.sf.zipme;
import java.io.IOException;
import java.io.OutputStream;

/** 
 * OutputStream that computes a checksum of data being written using a
 * supplied Checksum object.
 * @see Checksum
 * @author Tom Tromey
 * @date May 17, 1999
 */
public class CheckedOutputStream extends OutputStream {
  /** 
 * This is the subordinate <code>OutputStream</code> that this class
 * redirects its method calls to.
 */
  protected OutputStream out;
  /** 
 * Creates a new CheckInputStream on top of the supplied OutputStream
 * using the supplied Checksum.
 */
  public CheckedOutputStream(  OutputStream out,  Checksum cksum){
    this.out=out;
    this.sum=cksum;
  }
  /** 
 * Returns the Checksum object used. To get the data checksum computed so
 * far call <code>getChecksum.getValue()</code>.
 */
  public Checksum getChecksum(){
    return sum;
  }
  /** 
 * Writes one byte to the OutputStream and updates the Checksum.
 */
  public void write(  int bval) throws IOException {
    out.write(bval);
    sum.update(bval);
  }
  /** 
 * This method writes all the bytes in the specified array to the underlying
 * <code>OutputStream</code>.  It does this by calling the three parameter
 * version of this method - <code>write(byte[], int, int)</code> in this
 * class instead of writing to the underlying <code>OutputStream</code>
 * directly.  This allows most subclasses to avoid overriding this method.
 * @param buf The byte array to write bytes from
 * @exception IOException If an error occurs
 */
  public void write(  byte[] buf) throws IOException {
    write(buf,0,buf.length);
  }
  /** 
 * Writes the byte array to the OutputStream and updates the Checksum.
 */
  public void write(  byte[] buf,  int off,  int len) throws IOException {
    out.write(buf,off,len);
    sum.update(buf,off,len);
  }
  /** 
 * This method closes the underlying <code>OutputStream</code>.  Any
 * further attempts to write to this stream may throw an exception.
 * @exception IOException If an error occurs
 */
  public void close() throws IOException {
    flush();
    out.close();
  }
  /** 
 * This method attempt to flush all buffered output to be written to the
 * underlying output sink.
 * @exception IOException If an error occurs
 */
  public void flush() throws IOException {
    out.flush();
  }
  /** 
 * The checksum object. 
 */
  private Checksum sum;
}
