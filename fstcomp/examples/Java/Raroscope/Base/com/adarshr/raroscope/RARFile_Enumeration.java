////

/** 
 */
package com.adarshr.raroscope;
import java.io.IOException;
import java.util.Enumeration;
import java.util.NoSuchElementException;

public class RARFile_Enumeration implements Enumeration {
  /** 
 */
  private RARFile file;
  /** 
 * @param file
 */
  public RARFile_Enumeration(  RARFile file){
    this.file=file;
  }
  public RAREntry nextElement(){
    byte[] buf=new byte[32];
    RAREntry entry=null;
    try {
      this.file.available-=this.file.stream.read(buf);
      int type=buf[2] & 0xFF;
      if (type == 0x74) {
        entry=new RAREntry();
        long flags=this.file.getLong(buf,3,4);
        entry.setDirectory((flags & 0xE0) == 0xE0);
        long pSize=this.file.getLong(buf,7,10);
        long size=this.file.getLong(buf,11,14);
        if ((flags & 0x100) == 0x100) {
          byte[] hiBytes=new byte[8];
          this.file.available-=this.file.stream.read(hiBytes);
          pSize=this.file.getLong(hiBytes,0,4) << 32 | pSize;
          size=this.file.getLong(hiBytes,5,8) << 32 | size;
        }
        long hSize=this.file.getLong(buf,5,6);
        entry.setCompressedSize(pSize);
        entry.setSize(this.file.getLong(buf,11,14));
        this.hook28(buf,entry);
        this.hook29(buf,entry);
        entry.setTime(this.file.toDate(this.file.getLong(buf,20,23)));
        this.hook30(buf,entry);
        entry.setMethod(this.file.toMethod(buf[25] & 0xFF));
        long nSize=this.file.getLong(buf,26,27);
        byte[] name=new byte[(int)nSize];
        this.file.available-=this.file.stream.read(name);
        entry.setName(new String(name));
        this.file.available-=this.file.stream.skip(hSize - (32 + nSize) + pSize);
      }
    }
 catch (    IOException e) {
      throw new NoSuchElementException(e.getMessage());
    }
    if (entry == null) {
      throw new NoSuchElementException();
    }
    return entry;
  }
  public boolean hasMoreElements(){
    return this.file.available > 32;
  }
  protected void hook28(  byte[] buf,  RAREntry entry) throws IOException {
  }
  protected void hook29(  byte[] buf,  RAREntry entry) throws IOException {
  }
  protected void hook30(  byte[] buf,  RAREntry entry) throws IOException {
  }
}
