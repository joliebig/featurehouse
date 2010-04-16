//

package net.sf.zipme;
class GZIPInputStream {
  public CRC32 headCRC;
   public void hook(){
    crc=new CRC32();
    original();
  }
   protected void hook30(  byte[] buf,  int offset,  int numRead) throws IOException {
    if (numRead > 0)     crc.update(buf,offset,numRead);
    original(buf,offset,numRead);
  }
   public void hook3(  int crcval) throws IOException {
    if (crcval != ((int)headCRC.getValue() & 0xffff))     throw new IOException("Header CRC value mismatch");
    original(crcval);
  }
   public void hook2(  int CM){
    headCRC.update(CM);
    original(CM);
  }
   private void hook1(){
    headCRC=new CRC32();
    original();
  }
   public void hook4(  int crcval) throws IOException {
    if (crcval != (int)crc.getValue())     throw new IOException("GZIP crc sum mismatch, theirs \"" + Integer.toHexString(crcval) + "\" and ours \""+ Integer.toHexString((int)crc.getValue()));
    original(crcval);
  }
}
