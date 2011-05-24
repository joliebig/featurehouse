//

package net.sf.zipme;
import java.io.IOException;
import java.io.OutputStream;

class GZIPOutputStream_hook22 {
  GZIPOutputStream_hook22(  GZIPOutputStream _this,  byte[] gzipFooter){
    this._this=_this;
    this.gzipFooter=gzipFooter;
  }
  void execute(){
  }
  protected GZIPOutputStream _this;
  protected byte[] gzipFooter;
  protected int crcval;
}
