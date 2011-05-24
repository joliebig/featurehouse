//

package net.sf.zipme;
import java.io.IOException;
import java.io.InputStream;

/** 
 * This filter stream is used to decompress data compressed in the "deflate"
 * format. The "deflate" format is described in RFC 1951.
 * This stream may form the basis for other decompression filters, such
 * as the <code>GZIPInputStream</code>.
 * @author John Leuner
 * @author Tom Tromey
 * @since 1.1
 */
public class InflaterInputStream extends InputStream {
  /** 
 * This is the subordinate <code>InputStream</code> to which method calls
 * are redirected
 */
  protected InputStream in;
  /** 
 * Decompressor for this filter 
 */
  protected Inflater inf;
  /** 
 * Byte array used as a buffer 
 */
  protected byte[] buf;
  /** 
 * Size of buffer   
 */
  protected int len;
  private byte[] onebytebuffer=new byte[1];
  /** 
 * Create an InflaterInputStream with the default decompresseor
 * and a default buffer size.
 * @param in the InputStream to read bytes from
 */
  public InflaterInputStream(  InputStream in){
    this(in,new Inflater(),4096);
  }
  /** 
 * Create an InflaterInputStream with the specified decompresseor
 * and a default buffer size.
 * @param in the InputStream to read bytes from
 * @param inf the decompressor used to decompress data read from in
 */
  public InflaterInputStream(  InputStream in,  Inflater inf){
    this(in,inf,4096);
  }
  /** 
 * Create an InflaterInputStream with the specified decompresseor
 * and a specified buffer size.
 * @param in the InputStream to read bytes from
 * @param inf the decompressor used to decompress data read from in
 * @param size size of the buffer to use
 */
  public InflaterInputStream(  InputStream in,  Inflater inf,  int size){
    this.in=in;
    if (in == null)     throw new NullPointerException("in may not be null");
    if (inf == null)     throw new NullPointerException("inf may not be null");
    if (size < 0)     throw new IllegalArgumentException("size may not be negative");
    this.inf=inf;
    this.buf=new byte[size];
  }
  /** 
 * Returns 0 once the end of the stream (EOF) has been reached.
 * Otherwise returns 1.
 */
  public int available() throws IOException {
    if (inf == null)     throw new IOException("stream closed");
    return inf.finished() ? 0 : 1;
  }
  /** 
 * Closes the input stream
 */
  public synchronized void close() throws IOException {
    if (in != null)     in.close();
    in=null;
  }
  /** 
 * Fills the buffer with more data to decompress.
 */
  protected void fill() throws IOException {
    if (in == null)     throw new ZipException("InflaterInputStream is closed");
    len=in.read(buf,0,buf.length);
    if (len < 0)     throw new ZipException("Deflated stream ends early.");
    inf.setInput(buf,0,len);
  }
  /** 
 * Reads one byte of decompressed data.
 * The byte is in the lower 8 bits of the int.
 */
  public int read() throws IOException {
    int nread=read(onebytebuffer,0,1);
    if (nread > 0)     return onebytebuffer[0] & 0xff;
    return -1;
  }
  /** 
 * Calls the <code>read(byte[], int, int)</code> overloaded method.  
 * Note that 
 * this method does not redirect its call directly to a corresponding
 * method in <code>in</code>.  This allows subclasses to override only the
 * three argument version of <code>read</code>.
 * @param buf The buffer to read bytes into
 * @return The value retured from <code>in.read(byte[], int, int)</code>
 * @exception IOException If an error occurs
 */
  public int read(  byte[] buf) throws IOException {
    return read(buf,0,buf.length);
  }
  /** 
 * Decompresses data into the byte array
 * @param b the array to read and decompress data into
 * @param off the offset indicating where the data should be placed
 * @param len the number of bytes to decompress
 */
  public int read(  byte[] b,  int off,  int len) throws IOException {
    if (inf == null)     throw new IOException("stream closed");
    if (len == 0)     return 0;
    int count=0;
    for (; ; ) {
      try {
        count=inf.inflate(b,off,len);
      }
 catch (      DataFormatException dfe) {
        throw new ZipException(dfe.getMessage());
      }
      if (count > 0)       return count;
      if (inf.needsDictionary() | inf.finished())       return -1;
 else       if (inf.needsInput())       fill();
 else       throw new Error("Don't know what to do");
    }
  }
  /** 
 * Skip specified number of bytes of uncompressed data
 * @param n number of bytes to skip
 */
  public long skip(  long n) throws IOException {
    if (inf == null)     throw new IOException("stream closed");
    if (n < 0)     throw new IllegalArgumentException();
    if (n == 0)     return 0;
    int buflen=(int)Math.min(n,2048);
    byte[] tmpbuf=new byte[buflen];
    long skipped=0L;
    while (n > 0L) {
      int numread=read(tmpbuf,0,buflen);
      if (numread <= 0)       break;
      n-=numread;
      skipped+=numread;
      buflen=(int)Math.min(n,2048);
    }
    return skipped;
  }
  public boolean markSupported(){
    return false;
  }
  public void mark(  int readLimit){
  }
  public void reset() throws IOException {
    throw new IOException("reset not supported");
  }
}
