//

package net.sf.zipme;
class Deflater_deflate2 {
   protected void hook22(){
    chksum=_this.engine.getAdler();
    _this.engine.resetAdler();
    _this.pending.writeShortMSB(chksum >> 16);
    _this.pending.writeShortMSB(chksum & 0xffff);
    original();
  }
   protected void hook23(){
    adler=_this.engine.getAdler();
    _this.pending.writeShortMSB(adler >> 16);
    _this.pending.writeShortMSB(adler & 0xffff);
    original();
  }
}
