//

package com.adarshr.raroscope;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Calendar;
import java.util.Date;
import java.util.Enumeration;

/** 
 * Represents a RAR archive.
 * <p>
 * This class is used to enumerate the entries in a RAR file.
 * @author Adarsh Ramamurthy
 * @version 1.0, 10th March 2008
 */
public class RARFile {
  /** 
 * The underlying stream.
 */
  InputStream stream;
  /** 
 * Marker block of RAR archives. It is "Rar!" in reverse bytes.
 */
  private static final long MARKER=0x21726152L;
  /** 
 * To hold the available bytes in the stream.
 */
  long available;
  /** 
 * Constructs an instance of <tt>RARFile</tt> for performing operations
 * on the archive.
 * @param name the RAR file name.
 * @throws IOException in case of errors reading from the archive.
 */
  public RARFile(  String name) throws IOException {
    this(new File(name));
  }
  /** 
 * Constructs an instance of <tt>RARFile</tt> for performing operations
 * on the archive.
 * @param file the RAR file.
 * @throws IOException in case of errors reading from the archive.
 */
  public RARFile(  File file) throws IOException {
    this.stream=new FileInputStream(file);
    this.available=this.stream.available();
    byte[] headers=new byte[7 + 13];
    this.stream.read(headers);
    if (MARKER != getLong(headers,0,3)) {
      throw new IOException("Invalid RAR archive");
    }
  }
  /** 
 * Closes the archive.
 * @throws IOException in case of errors while closing.
 */
  public void close() throws IOException {
    this.stream.close();
  }
  /** 
 * Converts the input inverted array of bytes to a long representation.
 * @param bytes the byte array to be converted.
 * @return the long value.
 */
  protected long getLong(  byte[] bytes){
    long ret=0;
    long mask=0;
    for (int i=0; i < bytes.length; i++) {
      ret|=(bytes[i] & 0xFF) << (8 * i);
      mask=(mask << 8) | 0xFF;
    }
    return ret & mask;
  }
  /** 
 * Converts the input inverted array of bytes to a long representation.
 * Conversion is done inclusive of both the limits specified.
 * @param bytes the byte array to be converted.
 * @param start the index to start with.
 * @param end the end index.
 * @return the long value.
 */
  protected long getLong(  byte[] bytes,  int start,  int end){
    long ret=0;
    long mask=0;
    if (start < 0 || end >= bytes.length) {
      return ret;
    }
    for (int i=start, j=0; i <= end; i++, j++) {
      ret|=(bytes[i] & 0xFF) << (8 * j);
      mask=(mask << 8) | 0xFF;
    }
    return ret & mask;
  }
  /** 
 * Converts the DOS time to Java date.
 * @param dosTime MS DOS format time.
 * @return an instance of <tt>Date</tt>.
 */
  protected Date toDate(  long dosTime){
    Calendar calendar=Calendar.getInstance();
    calendar.set((int)(((dosTime >> 25) & 0x7f) + 1980),(int)(((dosTime >> 21) & 0x0f) - 1),(int)((dosTime >> 16) & 0x1f),(int)((dosTime >> 11) & 0x1f),(int)((dosTime >> 5) & 0x3f),(int)((dosTime << 1) & 0x3e));
    return calendar.getTime();
  }
  /** 
 * Translates the compression method into a string.
 * @param m the compression method number.
 * @return the compression method string.
 */
  protected String toMethod(  int m){
    String method=null;
switch (m) {
case 0x30:
      method="Storing";
    break;
case 0x31:
  method="Fastest Compression";
break;
case 0x32:
method="Fast Compression";
break;
case 0x33:
method="Normal Compression";
break;
case 0x34:
method="Good Compression";
break;
case 0x35:
method="Best Compression";
break;
default :
method="Unknown";
break;
}
return method;
}
}
