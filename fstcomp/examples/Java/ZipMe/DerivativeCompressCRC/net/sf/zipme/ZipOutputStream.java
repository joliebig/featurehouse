//

package net.sf.zipme;
class ZipOutputStream {
  private CRC32 crc=new CRC32();
   protected void hook41() throws IOException {
    crc.reset();
    original();
  }
   protected void hook42() throws IOException {
    if (curEntry.getCrc() < 0)     curEntry.setCrc(crc.getValue());
 else     if (curEntry.getCrc() != crc.getValue())     throw new ZipException("crc was " + toHexString(crc.getValue()) + ", but I expected "+ toHexString(curEntry.getCrc()));
    original();
  }
   protected void hook43(  byte[] b,  int off,  int len) throws IOException {
    crc.update(b,off,len);
    original(b,off,len);
  }
}
