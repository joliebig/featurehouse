//

package net.sf.zipme;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.util.Enumeration;
import java.util.Hashtable;
import java.io.File;
import java.io.FileInputStream;

/** 
 * This class represents a Zip archive.  You can ask for the contained
 * entries, or get an input stream for a file entry.  The entry is
 * automatically decompressed.
 * This class is thread safe:  You can open input streams for arbitrary
 * entries in different threads.
 * @author Jochen Hoenicke
 * @author Artur Biesiadowski
 */
public class ZipArchive implements ZipConstants {
  /** 
 * This field isn't defined in the JDK's ZipConstants, but should be.
 */
  static final int ENDNRD=4;
  private byte[] buf;
  private int off;
  private int len;
  private Hashtable entries;
  /** 
 * Opens a Zip archive reading the given byte array.
 * @exception ZipException if the byte array doesn't contain a valid
 * zip archive.
 */
  public ZipArchive(  byte[] b) throws ZipException {
    this(b,0,b.length);
  }
  /** 
 * Opens a Zip archive reading the given byte array.
 * @exception ZipException if the byte array doesn't contain a valid
 * zip archive.
 */
  public ZipArchive(  byte[] b,  int off,  int len) throws ZipException {
    if (off < 0 || len < 0 || off > b.length)     throw new IllegalArgumentException();
    buf=b;
    this.off=off;
    this.len=len;
    if (this.len > buf.length - this.off) {
      this.len=buf.length - this.off;
    }
    hook1();
  }
  public void hook1() throws ZipException {
  }
  /** 
 * Opens a Zip archive reading the given InputStream.
 * @exception IOException if a i/o error occured.
 * @exception ZipException if the stream doesn't contain a valid zip
 * archive.
 */
  public ZipArchive(  InputStream in) throws ZipException, IOException {
    ByteArrayOutputStream out=new ByteArrayOutputStream();
    byte[] b=new byte[1024];
    int l;
    while ((l=in.read(b)) > 0) {
      out.write(b,0,l);
    }
    buf=out.toByteArray();
    off=0;
    len=buf.length;
    hook1();
  }
  /** 
 * Read the central directory of a zip archive and fill the entries
 * array.  This is called exactly once when first needed. It is called
 * while holding the lock on <code>raf</code>.
 * @exception IOException if a i/o error occured.
 * @exception ZipException if the central directory is malformed 
 */
  private void readEntries() throws ZipException, IOException {
    ZipArchive_PartialInputStream inp=new ZipArchive_PartialInputStream(buf,off,len);
    int pos=len - ENDHDR;
    int top=Math.max(0,pos - 65536);
    do {
      if (pos < top)       throw new ZipException("central directory not found, probably not a zip archive");
      inp.seek(off + pos--);
    }
 while (inp.readLeInt() != ENDSIG);
    if (inp.skip(ENDTOT - ENDNRD) != ENDTOT - ENDNRD)     throw new EOFException();
    int count=inp.readLeShort();
    if (inp.skip(ENDOFF - ENDSIZ) != ENDOFF - ENDSIZ)     throw new EOFException();
    int centralOffset=inp.readLeInt();
    entries=new Hashtable(count + count / 2);
    inp.seek(off + centralOffset);
    for (int i=0; i < count; i++) {
      if (inp.readLeInt() != CENSIG)       throw new ZipException("Wrong Central Directory signature");
      inp.skip(6);
      int method=inp.readLeShort();
      int dostime=inp.readLeInt();
      int crc=inp.readLeInt();
      int csize=inp.readLeInt();
      int size=inp.readLeInt();
      int nameLen=inp.readLeShort();
      int extraLen=inp.readLeShort();
      int commentLen=inp.readLeShort();
      inp.skip(8);
      int offset=inp.readLeInt();
      String name=inp.readString(nameLen);
      ZipEntry entry=new ZipEntry(name);
      entry.setMethod(method);
      entry.setCrc(crc & 0xffffffffL);
      entry.setSize(size & 0xffffffffL);
      entry.setCompressedSize(csize & 0xffffffffL);
      entry.setDOSTime(dostime);
      if (extraLen > 0) {
        byte[] extra=new byte[extraLen];
        inp.readFully(extra);
        entry.setExtra(extra);
      }
      if (commentLen > 0) {
        entry.setComment(inp.readString(commentLen));
      }
      entry.offset=offset;
      entries.put(name,entry);
    }
  }
  /** 
 * Returns an enumeration of all Zip entries in this Zip archive.
 */
  public Enumeration entries(){
    try {
      return getEntries().elements();
    }
 catch (    IOException ioe) {
      return (new Hashtable()).elements();
    }
  }
  /** 
 * Reads entries when necessary.
 * @exception IOException when the entries could not be read.
 */
  private Hashtable getEntries() throws IOException {
    if (entries == null)     readEntries();
    return entries;
  }
  /** 
 * Searches for a zip entry in this archive with the given name.
 * @param name the name. May contain directory components separated by
 * slashes ('/').
 * @return the zip entry, or null if no entry with that name exists.
 */
  public ZipEntry getEntry(  String name){
    try {
      Hashtable entries=getEntries();
      ZipEntry entry=(ZipEntry)entries.get(name);
      if (entry == null && !name.endsWith("/"))       entry=(ZipEntry)entries.get(name + '/');
      return entry != null ? new ZipEntry(entry,name) : null;
    }
 catch (    IOException ioe) {
      return null;
    }
  }
  /** 
 * Returns the number of entries in this zip archive.
 */
  public int size(){
    try {
      return getEntries().size();
    }
 catch (    IOException ioe) {
      return 0;
    }
  }
}
