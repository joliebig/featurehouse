//

package net.sf.zipme;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;

/** 
 * This filter stream is used to decompress a "GZIP" format stream. 
 * The "GZIP" format is described in RFC 1952.
 * @author John Leuner
 * @author Tom Tromey
 * @since JDK 1.1
 */
public class GZIPInputStream extends InflaterInputStream {
  /** 
 * The magic number found at the start of a GZIP stream.
 */
  public static final int GZIP_MAGIC=0x8b1f;
  /** 
 * The mask for bit 0 of the flag byte.
 */
  static final int FTEXT=0x1;
  /** 
 * The mask for bit 1 of the flag byte.
 */
  static final int FHCRC=0x2;
  /** 
 * The mask for bit 2 of the flag byte.
 */
  static final int FEXTRA=0x4;
  /** 
 * The mask for bit 3 of the flag byte.
 */
  static final int FNAME=0x8;
  /** 
 * The mask for bit 4 of the flag byte.
 */
  static final int FCOMMENT=0x10;
  /** 
 * The CRC-32 checksum value for uncompressed data.
 */
  protected CRC32 crc;
  /** 
 * Indicates whether or not the end of the stream has been reached.
 */
  protected boolean eos;
  /** 
 * Indicates whether or not the GZIP header has been read in.
 */
  private boolean readGZIPHeader;
  /** 
 * Creates a GZIPInputStream with the default buffer size.
 * @param in The stream to read compressed data from 
 * (in GZIP format).
 * @throws IOException if an error occurs during an I/O operation.
 */
  public GZIPInputStream(  InputStream in) throws IOException {
    this(in,4096);
  }
  /** 
 * Creates a GZIPInputStream with the specified buffer size.
 * @param in The stream to read compressed data from 
 * (in GZIP format).
 * @param size The size of the buffer to use.
 * @throws IOException if an error occurs during an I/O operation.
 * @throws IllegalArgumentException if <code>size</code>
 * is less than or equal to 0.
 */
  public GZIPInputStream(  InputStream in,  int size) throws IOException {
    super(in,new Inflater(true),size);
    hook();
    readHeader();
  }
  public void hook(){
  }
  /** 
 * Closes the input stream.
 * @throws IOException if an error occurs during an I/O operation.
 */
  public void close() throws IOException {
    super.close();
  }
  /** 
 * Reads in GZIP-compressed data and stores it in uncompressed form
 * into an array of bytes.  The method will block until either
 * enough input data becomes available or the compressed stream
 * reaches its end.
 * @param buf the buffer into which the uncompressed data will
 * be stored.
 * @param offset the offset indicating where in <code>buf</code>
 * the uncompressed data should be placed.
 * @param len the number of uncompressed bytes to be read.
 */
  public int read(  byte[] buf,  int offset,  int len) throws IOException {
    if (!readGZIPHeader)     readHeader();
    if (eos)     return -1;
    int numRead=super.read(buf,offset,len);
    this.hook30(buf,offset,numRead);
    if (inf.finished())     readFooter();
    return numRead;
  }
  /** 
 * Reads in the GZIP header.
 */
  private void readHeader() throws IOException {
    hook1();
    int magic=in.read();
    if (magic < 0) {
      eos=true;
      return;
    }
    int magic2=in.read();
    if ((magic + (magic2 << 8)) != GZIP_MAGIC)     throw new IOException("Error in GZIP header, bad magic code");
    hook2(magic);
    hook2(magic2);
    int CM=in.read();
    if (CM != Deflater.DEFLATED)     throw new IOException("Error in GZIP header, data not in deflate format");
    hook2(CM);
    int flags=in.read();
    if (flags < 0)     throw new EOFException("Early EOF in GZIP header");
    hook2(flags);
    if ((flags & 0xd0) != 0)     throw new IOException("Reserved flag bits in GZIP header != 0");
    for (int i=0; i < 6; i++) {
      int readByte=in.read();
      if (readByte < 0)       throw new EOFException("Early EOF in GZIP header");
      hook2(readByte);
    }
    if ((flags & FEXTRA) != 0) {
      for (int i=0; i < 2; i++) {
        int readByte=in.read();
        if (readByte < 0)         throw new EOFException("Early EOF in GZIP header");
        hook2(readByte);
      }
      if (in.read() < 0 || in.read() < 0)       throw new EOFException("Early EOF in GZIP header");
      int len1, len2, extraLen;
      len1=in.read();
      len2=in.read();
      if ((len1 < 0) || (len2 < 0))       throw new EOFException("Early EOF in GZIP header");
      hook2(len1);
      hook2(len2);
      extraLen=(len1 << 8) | len2;
      for (int i=0; i < extraLen; i++) {
        int readByte=in.read();
        if (readByte < 0)         throw new EOFException("Early EOF in GZIP header");
        hook2(readByte);
      }
    }
    if ((flags & FNAME) != 0) {
      int readByte;
      while ((readByte=in.read()) > 0)       hook2(readByte);
      if (readByte < 0)       throw new EOFException("Early EOF in GZIP file name");
      hook2(readByte);
    }
    if ((flags & FCOMMENT) != 0) {
      int readByte;
      while ((readByte=in.read()) > 0)       hook2(readByte);
      if (readByte < 0)       throw new EOFException("Early EOF in GZIP comment");
      hook2(readByte);
    }
    if ((flags & FHCRC) != 0) {
      int tempByte;
      int crcval=in.read();
      if (crcval < 0)       throw new EOFException("Early EOF in GZIP header");
      tempByte=in.read();
      if (tempByte < 0)       throw new EOFException("Early EOF in GZIP header");
      crcval=(crcval << 8) | tempByte;
      hook3(crcval);
    }
    readGZIPHeader=true;
  }
  public void hook3(  int crcval) throws IOException {
  }
  public void hook2(  int CM){
  }
  private void hook1(){
  }
  private void readFooter() throws IOException {
    byte[] footer=new byte[8];
    int avail=inf.getRemaining();
    if (avail > 8)     avail=8;
    System.arraycopy(buf,len - inf.getRemaining(),footer,0,avail);
    int needed=8 - avail;
    while (needed > 0) {
      int count=in.read(footer,8 - needed,needed);
      if (count <= 0)       throw new EOFException("Early EOF in GZIP footer");
      needed-=count;
    }
    int crcval=(footer[0] & 0xff) | ((footer[1] & 0xff) << 8) | ((footer[2] & 0xff) << 16)| (footer[3] << 24);
    hook4(crcval);
    int total=(footer[4] & 0xff) | ((footer[5] & 0xff) << 8) | ((footer[6] & 0xff) << 16)| (footer[7] << 24);
    if (total != inf.getTotalOut())     throw new IOException("Number of bytes mismatch");
    eos=true;
  }
  public void hook4(  int crcval) throws IOException {
  }
  protected void hook30(  byte[] buf,  int offset,  int numRead) throws IOException {
  }
}
