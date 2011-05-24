//

package net.sf.zipme;
import java.util.Calendar;
import java.util.Date;

/** 
 * This class represents a member of a zip archive.  ZipFile and
 * ZipInputStream will give you instances of this class as information
 * about the members in an archive.  On the other hand ZipOutputStream
 * needs an instance of this class to create a new member.
 * @author Jochen Hoenicke 
 */
public class ZipEntry implements ZipConstants {
  private static final int KNOWN_SIZE=1;
  private static final int KNOWN_CSIZE=2;
  private static final int KNOWN_CRC=4;
  private static final int KNOWN_TIME=8;
  private static final int KNOWN_EXTRA=16;
  private static Calendar cal;
  private String name;
  private int size;
  private long compressedSize=-1;
  private int crc;
  private int dostime;
  private short known=0;
  private short method=-1;
  private byte[] extra=null;
  private String comment=null;
  int flags;
  public int offset;
  /** 
 * Compression method.  This method doesn't compress at all.
 */
  public static final int STORED=0;
  /** 
 * Compression method.  This method uses the Deflater.
 */
  public static final int DEFLATED=8;
  /** 
 * Creates a zip entry with the given name.
 * @param name the name. May include directory components separated
 * by '/'.
 * @exception NullPointerException when name is null.
 * @exception IllegalArgumentException when name is bigger then 65535 chars.
 */
  public ZipEntry(  String name){
    int length=name.length();
    if (length > 65535)     throw new IllegalArgumentException("name length is " + length);
    this.name=name;
  }
  /** 
 * Creates a copy of the given zip entry.
 * @param e the entry to copy.
 */
  public ZipEntry(  ZipEntry e){
    this(e,e.name);
  }
  public ZipEntry(  ZipEntry e,  String name){
    this.name=name;
    known=e.known;
    size=e.size;
    compressedSize=e.compressedSize;
    crc=e.crc;
    dostime=e.dostime;
    method=e.method;
    extra=e.extra;
    comment=e.comment;
  }
  final void setDOSTime(  int dostime){
    this.dostime=dostime;
    known|=KNOWN_TIME;
  }
  final int getDOSTime(){
    if ((known & KNOWN_TIME) == 0)     return 0;
 else     return dostime;
  }
  /** 
 * Returns the entry name.  The path components in the entry are
 * always separated by slashes ('/').  
 */
  public String getName(){
    return name;
  }
  /** 
 * Sets the time of last modification of the entry.
 * @param time the time of last modification of the entry.
 */
  public void setTime(  long time){
    Calendar cal=getCalendar();
synchronized (cal) {
      cal.setTime(new Date(time));
      dostime=(cal.get(Calendar.YEAR) - 1980 & 0x7f) << 25 | (cal.get(Calendar.MONTH) + 1) << 21 | (cal.get(Calendar.DAY_OF_MONTH)) << 16 | (cal.get(Calendar.HOUR_OF_DAY)) << 11 | (cal.get(Calendar.MINUTE)) << 5 | (cal.get(Calendar.SECOND)) >> 1;
    }
    this.known|=KNOWN_TIME;
  }
  /** 
 * Gets the time of last modification of the entry.
 * @return the time of last modification of the entry, or -1 if unknown.
 */
  public long getTime(){
    parseExtra();
    if ((known & KNOWN_TIME) == 0)     return -1;
    int sec=2 * (dostime & 0x1f);
    int min=(dostime >> 5) & 0x3f;
    int hrs=(dostime >> 11) & 0x1f;
    int day=(dostime >> 16) & 0x1f;
    int mon=((dostime >> 21) & 0xf) - 1;
    int year=((dostime >> 25) & 0x7f) + 1980;
    try {
      cal=getCalendar();
synchronized (cal) {
        cal.set(Calendar.YEAR,year);
        cal.set(Calendar.MONTH,mon);
        cal.set(Calendar.DAY_OF_MONTH,day);
        cal.set(Calendar.HOUR_OF_DAY,hrs);
        cal.set(Calendar.MINUTE,min);
        cal.set(Calendar.SECOND,sec);
        return cal.getTime().getTime();
      }
    }
 catch (    RuntimeException ex) {
      known&=~KNOWN_TIME;
      return -1;
    }
  }
  private static synchronized Calendar getCalendar(){
    if (cal == null)     cal=Calendar.getInstance();
    return cal;
  }
  /** 
 * Sets the size of the uncompressed data.
 * @exception IllegalArgumentException if size is not in 0..0xffffffffL
 */
  public void setSize(  long size){
    if ((size & 0xffffffff00000000L) != 0)     throw new IllegalArgumentException();
    this.size=(int)size;
    this.known|=KNOWN_SIZE;
  }
  /** 
 * Gets the size of the uncompressed data.
 * @return the size or -1 if unknown.
 */
  public long getSize(){
    return (known & KNOWN_SIZE) != 0 ? size & 0xffffffffL : -1L;
  }
  /** 
 * Sets the size of the compressed data.
 */
  public void setCompressedSize(  long csize){
    this.compressedSize=csize;
  }
  /** 
 * Gets the size of the compressed data.
 * @return the size or -1 if unknown.
 */
  public long getCompressedSize(){
    return compressedSize;
  }
  /** 
 * Sets the crc of the uncompressed data.
 * @exception IllegalArgumentException if crc is not in 0..0xffffffffL
 */
  public void setCrc(  long crc){
    if ((crc & 0xffffffff00000000L) != 0)     throw new IllegalArgumentException();
    this.crc=(int)crc;
    this.known|=KNOWN_CRC;
  }
  /** 
 * Gets the crc of the uncompressed data.
 * @return the crc or -1 if unknown.
 */
  public long getCrc(){
    return (known & KNOWN_CRC) != 0 ? crc & 0xffffffffL : -1L;
  }
  /** 
 * Sets the compression method.  Only DEFLATED and STORED are
 * supported.
 * @exception IllegalArgumentException if method is not supported.
 * @see ZipOutputStream#DEFLATED
 * @see ZipOutputStream#STORED 
 */
  public void setMethod(  int method){
    if (method != ZipOutputStream.STORED && method != ZipOutputStream.DEFLATED)     throw new IllegalArgumentException();
    this.method=(short)method;
  }
  /** 
 * Gets the compression method.  
 * @return the compression method or -1 if unknown.
 */
  public int getMethod(){
    return method;
  }
  /** 
 * Sets the extra data.
 * @exception IllegalArgumentException if extra is longer than 0xffff bytes.
 */
  public void setExtra(  byte[] extra){
    if (extra == null) {
      this.extra=null;
      return;
    }
    if (extra.length > 0xffff)     throw new IllegalArgumentException();
    this.extra=extra;
  }
  private void parseExtra(){
    if ((known & KNOWN_EXTRA) != 0)     return;
    if (extra == null) {
      known|=KNOWN_EXTRA;
      return;
    }
    try {
      int pos=0;
      while (pos < extra.length) {
        int sig=(extra[pos++] & 0xff) | (extra[pos++] & 0xff) << 8;
        int len=(extra[pos++] & 0xff) | (extra[pos++] & 0xff) << 8;
        if (sig == 0x5455) {
          int flags=extra[pos];
          if ((flags & 1) != 0) {
            long time=((extra[pos + 1] & 0xff) | (extra[pos + 2] & 0xff) << 8 | (extra[pos + 3] & 0xff) << 16 | (extra[pos + 4] & 0xff) << 24);
            setTime(time);
          }
        }
        pos+=len;
      }
    }
 catch (    ArrayIndexOutOfBoundsException ex) {
    }
    known|=KNOWN_EXTRA;
    return;
  }
  /** 
 * Gets the extra data.
 * @return the extra data or null if not set.
 */
  public byte[] getExtra(){
    return extra;
  }
  /** 
 * Sets the entry comment.
 * @exception IllegalArgumentException if comment is longer than 0xffff.
 */
  public void setComment(  String comment){
    if (comment != null && comment.length() > 0xffff)     throw new IllegalArgumentException();
    this.comment=comment;
  }
  /** 
 * Gets the comment.
 * @return the comment or null if not set.
 */
  public String getComment(){
    return comment;
  }
  /** 
 * Gets true, if the entry is a directory.  This is solely
 * determined by the name, a trailing slash '/' marks a directory.  
 */
  public boolean isDirectory(){
    int nlen=name.length();
    return nlen > 0 && name.charAt(nlen - 1) == '/';
  }
  /** 
 * Gets the string representation of this ZipEntry.  This is just
 * the name as returned by getName().
 */
  public String toString(){
    return name;
  }
  /** 
 * Gets the hashCode of this ZipEntry.  This is just the hashCode
 * of the name.  Note that the equals method isn't changed, though.
 */
  public int hashCode(){
    return name.hashCode();
  }
}
