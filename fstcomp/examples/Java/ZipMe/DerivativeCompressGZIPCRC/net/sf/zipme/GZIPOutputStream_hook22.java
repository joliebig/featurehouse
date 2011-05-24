//

package net.sf.zipme;
class GZIPOutputStream_hook22 {
   void execute(){
    crcval=(int)(_this.crc.getValue() & 0xffffffff);
    gzipFooter[0]=(byte)crcval;
    gzipFooter[1]=(byte)(crcval >> 8);
    gzipFooter[2]=(byte)(crcval >> 16);
    gzipFooter[3]=(byte)(crcval >> 24);
    original();
  }
}
