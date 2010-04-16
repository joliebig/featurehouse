//

package net.sf.zipme;

/** 
 * Contains the output from the Inflation process.
 * We need to have a window so that we can refer backwards into the output stream
 * to repeat stuff.
 * @author John Leuner
 * @since 1.1
 */
class OutputWindow {
  private static final int WINDOW_SIZE=1 << 15;
  private static final int WINDOW_MASK=WINDOW_SIZE - 1;
  private byte[] window=new byte[WINDOW_SIZE];
  private int window_end=0;
  private int window_filled=0;
  public void write(  int abyte){
    if (window_filled++ == WINDOW_SIZE)     throw new IllegalStateException("Window full");
    window[window_end++]=(byte)abyte;
    window_end&=WINDOW_MASK;
  }
  private void slowRepeat(  int rep_start,  int len,  int dist){
    while (len-- > 0) {
      window[window_end++]=window[rep_start++];
      window_end&=WINDOW_MASK;
      rep_start&=WINDOW_MASK;
    }
  }
  public void repeat(  int len,  int dist){
    if ((window_filled+=len) > WINDOW_SIZE)     throw new IllegalStateException("Window full");
    int rep_start=(window_end - dist) & WINDOW_MASK;
    int border=WINDOW_SIZE - len;
    if (rep_start <= border && window_end < border) {
      if (len <= dist) {
        System.arraycopy(window,rep_start,window,window_end,len);
        window_end+=len;
      }
 else {
        while (len-- > 0)         window[window_end++]=window[rep_start++];
      }
    }
 else     slowRepeat(rep_start,len,dist);
  }
  public int copyStored(  StreamManipulator input,  int len){
    len=Math.min(Math.min(len,WINDOW_SIZE - window_filled),input.getAvailableBytes());
    int copied;
    int tailLen=WINDOW_SIZE - window_end;
    if (len > tailLen) {
      copied=input.copyBytes(window,window_end,tailLen);
      if (copied == tailLen)       copied+=input.copyBytes(window,0,len - tailLen);
    }
 else     copied=input.copyBytes(window,window_end,len);
    window_end=(window_end + copied) & WINDOW_MASK;
    window_filled+=copied;
    return copied;
  }
  public void copyDict(  byte[] dict,  int offset,  int len){
    if (window_filled > 0)     throw new IllegalStateException();
    if (len > WINDOW_SIZE) {
      offset+=len - WINDOW_SIZE;
      len=WINDOW_SIZE;
    }
    System.arraycopy(dict,offset,window,0,len);
    window_end=len & WINDOW_MASK;
  }
  public int getFreeSpace(){
    return WINDOW_SIZE - window_filled;
  }
  public int getAvailable(){
    return window_filled;
  }
  public int copyOutput(  byte[] output,  int offset,  int len){
    int copy_end=window_end;
    if (len > window_filled)     len=window_filled;
 else     copy_end=(window_end - window_filled + len) & WINDOW_MASK;
    int copied=len;
    int tailLen=len - copy_end;
    if (tailLen > 0) {
      System.arraycopy(window,WINDOW_SIZE - tailLen,output,offset,tailLen);
      offset+=tailLen;
      len=copy_end;
    }
    System.arraycopy(window,copy_end - len,output,offset,len);
    window_filled-=copied;
    if (window_filled < 0)     throw new IllegalStateException();
    return copied;
  }
  public void reset(){
    window_filled=window_end=0;
  }
}
