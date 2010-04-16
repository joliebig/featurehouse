//

package net.sf.zipme;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.util.Enumeration;
import java.util.Vector;

/** 
 * This is a FilterOutputStream that writes the files into a zip
 * archive one after another.  It has a special method to start a new
 * zip entry.  The zip entries contains information about the file name
 * size, compressed size, CRC, etc.
 * It includes support for STORED and DEFLATED entries.
 * This class is not thread safe.
 * @author Jochen Hoenicke 
 */
public class ZipOutputStream extends DeflaterOutputStream implements ZipConstants {
  /** 
 * Compression method.  This method doesn't compress at all.
 */
  public static final int STORED=0;
  /** 
 * Compression method.  This method uses the Deflater.
 */
  public static final int DEFLATED=8;
  /** 
 * Creates a new Zip output stream, writing a zip archive.
 * @param out the output stream to which the zip archive is written.
 */
  public ZipOutputStream(  OutputStream out){
    super(out,new Deflater(Deflater.DEFAULT_COMPRESSION,true));
  }
}
