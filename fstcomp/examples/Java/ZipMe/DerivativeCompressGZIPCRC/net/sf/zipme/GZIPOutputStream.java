//

package net.sf.zipme;
class GZIPOutputStream {
   public void hook(){
    crc=new CRC32();
    original();
  }
   protected void hook31(  byte[] buf,  int off,  int len) throws IOException {
    crc.update(buf,off,len);
    original(buf,off,len);
  }
}
