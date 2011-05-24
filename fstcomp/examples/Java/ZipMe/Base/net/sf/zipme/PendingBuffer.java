//

package net.sf.zipme;

/** 
 * This class is general purpose class for writing data to a buffer.
 * It allows you to write bits as well as bytes 
 * Based on DeflaterPending.java
 * @author Jochen Hoenicke
 * @date Jan 5, 2000 
 */
class PendingBuffer {
  protected byte[] buf;
  int start;
  int end;
  int bits;
  int bitCount;
  public PendingBuffer(){
    this(4096);
  }
  public PendingBuffer(  int bufsize){
    buf=new byte[bufsize];
  }
  public final void reset(){
    start=end=bitCount=0;
  }
  public final void writeByte(  int b){
    if (DeflaterConstants.DEBUGGING && start != 0)     throw new IllegalStateException();
    buf[end++]=(byte)b;
  }
  public final void writeShort(  int s){
    if (DeflaterConstants.DEBUGGING && start != 0)     throw new IllegalStateException();
    buf[end++]=(byte)s;
    buf[end++]=(byte)(s >> 8);
  }
  public final void writeInt(  int s){
    if (DeflaterConstants.DEBUGGING && start != 0)     throw new IllegalStateException();
    buf[end++]=(byte)s;
    buf[end++]=(byte)(s >> 8);
    buf[end++]=(byte)(s >> 16);
    buf[end++]=(byte)(s >> 24);
  }
  public final void writeBlock(  byte[] block,  int offset,  int len){
    if (DeflaterConstants.DEBUGGING && start != 0)     throw new IllegalStateException();
    System.arraycopy(block,offset,buf,end,len);
    end+=len;
  }
  public final int getBitCount(){
    return bitCount;
  }
  public final void alignToByte(){
    if (DeflaterConstants.DEBUGGING && start != 0)     throw new IllegalStateException();
    if (bitCount > 0) {
      buf[end++]=(byte)bits;
      if (bitCount > 8)       buf[end++]=(byte)(bits >>> 8);
    }
    bits=0;
    bitCount=0;
  }
  public final void writeBits(  int b,  int count){
    if (DeflaterConstants.DEBUGGING && start != 0)     throw new IllegalStateException();
    if (DeflaterConstants.DEBUGGING)     System.err.println("writeBits(" + Integer.toHexString(b) + ","+ count+ ")");
    bits|=b << bitCount;
    bitCount+=count;
    if (bitCount >= 16) {
      buf[end++]=(byte)bits;
      buf[end++]=(byte)(bits >>> 8);
      bits>>>=16;
      bitCount-=16;
    }
  }
  public final void writeShortMSB(  int s){
    if (DeflaterConstants.DEBUGGING && start != 0)     throw new IllegalStateException();
    buf[end++]=(byte)(s >> 8);
    buf[end++]=(byte)s;
  }
  public final boolean isFlushed(){
    return end == 0;
  }
  /** 
 * Flushes the pending buffer into the given output array.  If the
 * output array is to small, only a partial flush is done.
 * @param output the output array;
 * @param offset the offset into output array;
 * @param length the maximum number of bytes to store;
 * @exception IndexOutOfBoundsException if offset or length are
 * invalid.
 */
  public final int flush(  byte[] output,  int offset,  int length){
    if (bitCount >= 8) {
      buf[end++]=(byte)bits;
      bits>>>=8;
      bitCount-=8;
    }
    if (length > end - start) {
      length=end - start;
      System.arraycopy(buf,start,output,offset,length);
      start=0;
      end=0;
    }
 else {
      System.arraycopy(buf,start,output,offset,length);
      start+=length;
    }
    return length;
  }
  /** 
 * Flushes the pending buffer and returns that data in a new array
 * @return the output stream
 */
  public final byte[] toByteArray(){
    byte[] ret=new byte[end - start];
    System.arraycopy(buf,start,ret,0,ret.length);
    start=0;
    end=0;
    return ret;
  }
}
