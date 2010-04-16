//

package net.sf.zipme;
class ZipArchive {
  /** 
 * Creates an input stream reading the given zip entry as
 * uncompressed data.  Normally zip entry should be an entry
 * returned by getEntry() or entries().
 * This implementation returns null if the requested entry does not
 * exist.  This decision is not obviously correct, however, it does
 * appear to mirror Sun's implementation, and it is consistant with
 * their javadoc.  On the other hand, the old JCL book, 2nd Edition,
 * claims that this should return a "non-null ZIP entry".  We have
 * chosen for now ignore the old book, as modern versions of Ant (an
 * important application) depend on this behaviour.  See discussion
 * in this thread:
 * http://gcc.gnu.org/ml/java-patches/2004-q2/msg00602.html
 * @param entry the entry to create an InputStream for.
 * @return the input stream, or null if the requested entry does not exist.
 * @exception IOException if a i/o error occured.
 * @exception ZipException if the Zip archive is malformed.  
 */
   public InputStream getInputStream(  ZipEntry entry) throws IOException {
    Hashtable entries=getEntries();
    String name=entry.getName();
    ZipEntry zipEntry=(ZipEntry)entries.get(name);
    if (zipEntry == null)     return null;
    ZipArchive_PartialInputStream inp=new ZipArchive_PartialInputStream(buf,off,len);
    inp.seek(off + zipEntry.offset);
    if (inp.readLeInt() != LOCSIG)     throw new ZipException("Wrong Local header signature: " + name);
    inp.skip(4);
    if (zipEntry.getMethod() != inp.readLeShort())     throw new ZipException("Compression method mismatch: " + name);
    inp.skip(16);
    int nameLen=inp.readLeShort();
    int extraLen=inp.readLeShort();
    inp.skip(nameLen + extraLen);
    inp.setLength((int)zipEntry.getCompressedSize());
    int method=zipEntry.getMethod();
switch (method) {
case ZipOutputStream.STORED:
      return inp;
case ZipOutputStream.DEFLATED:
    inp.addDummyByte();
  final Inflater inf=new Inflater(true);
final int sz=(int)entry.getSize();
return new ZipArchive_InflaterInputStream(inp,inf,sz);
default :
throw new ZipException("Unknown compression method " + method);
}
}
}
