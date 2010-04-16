//

/** 
 */
package net.sf.zipme;
import java.io.IOException;
import java.io.InputStream;

public class ZipArchive_InflaterInputStream extends InflaterInputStream {
  private final int sz;
  public ZipArchive_InflaterInputStream(  InputStream in,  Inflater inf,  int sz){
    super(in,inf);
    this.sz=sz;
  }
  public int available() throws IOException {
    if (sz == -1)     return super.available();
    if (super.available() != 0)     return sz - inf.getTotalOut();
    return 0;
  }
}
