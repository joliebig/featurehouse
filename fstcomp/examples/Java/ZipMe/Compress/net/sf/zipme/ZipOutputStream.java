//

package net.sf.zipme;
class ZipOutputStream {
  private Vector entries=new Vector();
  private ZipEntry curEntry=null;
  private int curMethod;
  private int size;
  private int offset=0;
  private byte[] zipComment=new byte[0];
  private int defaultMethod=DEFLATED;
  /** 
 * Our Zip version is hard coded to 1.0 resp. 2.0
 */
  private static final int ZIP_STORED_VERSION=10;
  private static final int ZIP_DEFLATED_VERSION=20;
  /** 
 * Table for calculating digits, used in Character, Long, and Integer.
 */
  static final char[] digits={'0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'};
  /** 
 * Set the zip file comment.
 * @param comment the comment.
 * @exception IllegalArgumentException if encoding of comment is
 * longer than 0xffff bytes.
 */
   public void setComment(  String comment){
    byte[] commentBytes;
    try {
      commentBytes=comment.getBytes("UTF-8");
    }
 catch (    UnsupportedEncodingException uee) {
      throw new Error(uee.toString());
    }
    if (commentBytes.length > 0xffff)     throw new IllegalArgumentException("Comment too long.");
    zipComment=commentBytes;
  }
  /** 
 * Sets default compression method.  If the Zip entry specifies
 * another method its method takes precedence.
 * @param method the method.
 * @exception IllegalArgumentException if method is not supported.
 * @see #STORED
 * @see #DEFLATED
 */
   public void setMethod(  int method){
    if (method != STORED && method != DEFLATED)     throw new IllegalArgumentException("Method not supported.");
    defaultMethod=method;
  }
  /** 
 * Sets default compression level.  The new level will be activated
 * immediately.  
 * @exception IllegalArgumentException if level is not supported.
 * @see Deflater
 */
   public void setLevel(  int level){
    def.setLevel(level);
  }
  /** 
 * Write an unsigned short in little endian byte order.
 */
   private void writeLeShort(  int value) throws IOException {
    out.write(value & 0xff);
    out.write((value >> 8) & 0xff);
  }
  /** 
 * Write an int in little endian byte order.
 */
   private void writeLeInt(  int value) throws IOException {
    writeLeShort(value);
    writeLeShort(value >> 16);
  }
  /** 
 * Write a long value as an int.  Some of the zip constants
 * are declared as longs even though they fit perfectly well
 * into integers.
 */
   private void writeLeInt(  long value) throws IOException {
    writeLeInt((int)value);
  }
  /** 
 * Starts a new Zip entry. It automatically closes the previous
 * entry if present.  If the compression method is stored, the entry
 * must have a valid size and crc, otherwise all elements (except
 * name) are optional, but must be correct if present.  If the time
 * is not set in the entry, the current time is used.
 * @param entry the entry.
 * @exception IOException if an I/O error occured.
 * @exception ZipException if stream was finished.
 */
   public void putNextEntry(  ZipEntry entry) throws IOException {
    if (entries == null)     throw new ZipException("ZipOutputStream was finished");
    int method=entry.getMethod();
    int flags=0;
    if (method == -1)     method=defaultMethod;
    if (method == STORED) {
      if (entry.getCompressedSize() >= 0) {
        if (entry.getSize() < 0)         entry.setSize(entry.getCompressedSize());
 else         if (entry.getSize() != entry.getCompressedSize())         throw new ZipException("Method STORED, but compressed size != size");
      }
 else       entry.setCompressedSize(entry.getSize());
      if (entry.getSize() < 0)       throw new ZipException("Method STORED, but size not set");
      if (entry.getCrc() < 0)       throw new ZipException("Method STORED, but crc not set");
    }
 else     if (method == DEFLATED) {
      if (entry.getCompressedSize() < 0 || entry.getSize() < 0 || entry.getCrc() < 0)       flags|=8;
    }
    if (curEntry != null)     closeEntry();
    if (entry.getTime() < 0)     entry.setTime(System.currentTimeMillis());
    entry.flags=flags;
    entry.offset=offset;
    entry.setMethod(method);
    curMethod=method;
    writeLeInt(LOCSIG);
    writeLeShort(method == STORED ? ZIP_STORED_VERSION : ZIP_DEFLATED_VERSION);
    writeLeShort(flags);
    writeLeShort(method);
    writeLeInt(entry.getDOSTime());
    if ((flags & 8) == 0) {
      writeLeInt((int)entry.getCrc());
      writeLeInt((int)entry.getCompressedSize());
      writeLeInt((int)entry.getSize());
    }
 else {
      writeLeInt(0);
      writeLeInt(0);
      writeLeInt(0);
    }
    byte[] name;
    try {
      name=entry.getName().getBytes("UTF-8");
    }
 catch (    UnsupportedEncodingException uee) {
      throw new Error(uee.toString());
    }
    if (name.length > 0xffff)     throw new ZipException("Name too long.");
    byte[] extra=entry.getExtra();
    if (extra == null)     extra=new byte[0];
    writeLeShort(name.length);
    writeLeShort(extra.length);
    out.write(name);
    out.write(extra);
    offset+=LOCHDR + name.length + extra.length;
    curEntry=entry;
    this.hook41();
    if (method == DEFLATED)     def.reset();
    size=0;
  }
  /** 
 * Closes the current entry.
 * @exception IOException if an I/O error occured.
 * @exception ZipException if no entry is active.
 */
   public void closeEntry() throws IOException {
    if (curEntry == null)     throw new ZipException("No open entry");
    if (curMethod == DEFLATED)     super.finish();
    int csize=curMethod == DEFLATED ? def.getTotalOut() : size;
    if (curEntry.getSize() < 0)     curEntry.setSize(size);
 else     if (curEntry.getSize() != size)     throw new ZipException("size was " + size + ", but I expected "+ curEntry.getSize());
    if (curEntry.getCompressedSize() < 0)     curEntry.setCompressedSize(csize);
 else     if (curEntry.getCompressedSize() != csize)     throw new ZipException("compressed size was " + csize + ", but I expected "+ curEntry.getSize());
    this.hook42();
    offset+=csize;
    if (curMethod == DEFLATED && (curEntry.flags & 8) != 0) {
      writeLeInt(EXTSIG);
      writeLeInt((int)curEntry.getCrc());
      writeLeInt((int)curEntry.getCompressedSize());
      writeLeInt((int)curEntry.getSize());
      offset+=EXTHDR;
    }
    entries.addElement(curEntry);
    curEntry=null;
  }
  /** 
 * Writes the given buffer to the current entry.
 * @exception IOException if an I/O error occured.
 * @exception ZipException if no entry is active.
 */
   public void write(  byte[] b,  int off,  int len) throws IOException {
    if (curEntry == null)     throw new ZipException("No open entry.");
switch (curMethod) {
case DEFLATED:
      super.write(b,off,len);
    break;
case STORED:
  out.write(b,off,len);
break;
}
this.hook43(b,off,len);
size+=len;
}
/** 
 * Finishes the stream.  This will write the central directory at the
 * end of the zip file and flush the stream.
 * @exception IOException if an I/O error occured.
 */
 public void finish() throws IOException {
if (entries == null) return;
if (curEntry != null) closeEntry();
int numEntries=0;
int sizeEntries=0;
Enumeration e=entries.elements();
while (e.hasMoreElements()) {
ZipEntry entry=(ZipEntry)e.nextElement();
int method=entry.getMethod();
writeLeInt(CENSIG);
writeLeShort(method == STORED ? ZIP_STORED_VERSION : ZIP_DEFLATED_VERSION);
writeLeShort(method == STORED ? ZIP_STORED_VERSION : ZIP_DEFLATED_VERSION);
writeLeShort(entry.flags);
writeLeShort(method);
writeLeInt(entry.getDOSTime());
writeLeInt((int)entry.getCrc());
writeLeInt((int)entry.getCompressedSize());
writeLeInt((int)entry.getSize());
byte[] name;
try {
name=entry.getName().getBytes("UTF-8");
}
 catch (UnsupportedEncodingException uee) {
throw new Error(uee.toString());
}
if (name.length > 0xffff) throw new ZipException("Name too long.");
byte[] extra=entry.getExtra();
if (extra == null) extra=new byte[0];
String str=entry.getComment();
byte[] comment;
try {
comment=str != null ? str.getBytes("UTF-8") : new byte[0];
}
 catch (UnsupportedEncodingException uee) {
throw new Error(uee.toString());
}
if (comment.length > 0xffff) throw new ZipException("Comment too long.");
writeLeShort(name.length);
writeLeShort(extra.length);
writeLeShort(comment.length);
writeLeShort(0);
writeLeShort(0);
writeLeInt(0);
writeLeInt(entry.offset);
out.write(name);
out.write(extra);
out.write(comment);
numEntries++;
sizeEntries+=CENHDR + name.length + extra.length+ comment.length;
}
writeLeInt(ENDSIG);
writeLeShort(0);
writeLeShort(0);
writeLeShort(numEntries);
writeLeShort(numEntries);
writeLeInt(sizeEntries);
writeLeInt(offset);
writeLeShort(zipComment.length);
out.write(zipComment);
out.flush();
entries=null;
}
/** 
 * Converts the <code>long</code> to a <code>String</code> assuming it is
 * unsigned in base 16.
 * @param l the <code>long</code> to convert to <code>String</code>
 * @return the <code>String</code> representation of the argument
 */
 private static String toHexString(long num){
if (num >= 0 && (int)num == num) return Integer.toHexString((int)num);
int mask=(1 << 4) - 1;
char[] buffer=new char[64];
int i=64;
do {
buffer[--i]=digits[(int)num & mask];
num>>>=4;
}
 while (num != 0);
return new String(buffer,i,64 - i);
}
 protected void hook41() throws IOException {
}
 protected void hook42() throws IOException {
}
 protected void hook43(byte[] b,int off,int len) throws IOException {
}
}
