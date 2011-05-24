//

package net.sf.zipme;
import java.io.ByteArrayInputStream;
import java.io.EOFException;
import java.io.UnsupportedEncodingException;

public final class ZipArchive_PartialInputStream extends ByteArrayInputStream {
  private int dummyByteCount;
  public ZipArchive_PartialInputStream(  byte[] buffer,  int offset,  int length){
    super(buffer,offset,length);
  }
  synchronized void setLength(  int length){
    count=pos + length;
    if (count > buf.length)     count=buf.length;
  }
  public synchronized int read(){
    if (dummyByteCount > 0 && pos == count) {
      dummyByteCount=0;
      pos++;
      return 0;
    }
    return super.read();
  }
  public synchronized int read(  byte[] buffer,  int offset,  int length){
    int numBytes=super.read(buffer,offset,length);
    if (dummyByteCount > 0 && numBytes < length) {
      dummyByteCount=0;
      if (pos < count)       buffer[offset + numBytes++]=buf[pos++];
 else       if (pos == count) {
        if (numBytes == -1)         numBytes=0;
        buffer[offset + numBytes++]=0;
        pos++;
      }
    }
    return numBytes;
  }
  void seek(  int newpos){
    pos=newpos;
  }
  void readFully(  byte[] buf) throws EOFException {
    if (read(buf,0,buf.length) != buf.length)     throw new EOFException();
  }
  void readFully(  byte[] buf,  int off,  int len) throws EOFException {
    if (read(buf,off,len) != len)     throw new EOFException();
  }
  synchronized int readLeShort() throws EOFException {
    int b0=read();
    int b1=read();
    if (b1 == -1)     throw new EOFException();
    return (b0 & 0xff) | (b1 & 0xff) << 8;
  }
  synchronized int readLeInt() throws EOFException {
    int b0=read();
    int b1=read();
    int b2=read();
    int b3=read();
    if (b3 == -1)     throw new EOFException();
    return ((b0 & 0xff) | (b1 & 0xff) << 8) | ((b2 & 0xff) | (b3 & 0xff) << 8) << 16;
  }
  synchronized String readString(  int length) throws EOFException {
    if (length > count - pos)     throw new EOFException();
    try {
      byte[] b=new byte[length];
      readFully(b);
      return new String(b,0,length,"UTF-8");
    }
 catch (    UnsupportedEncodingException uee) {
      throw new Error(uee.toString());
    }
  }
  public void addDummyByte(){
    dummyByteCount=1;
  }
}
