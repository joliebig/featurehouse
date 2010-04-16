//

package net.sf.zipme;
class DeflaterEngine {
  /** 
 * The adler checksum 
 */
  private Adler32 adler;
   public final void resetAdler(){
    adler.reset();
  }
   public final int getAdler(){
    int chksum=(int)adler.getValue();
    return chksum;
  }
   protected void hook26(){
    adler=new Adler32();
    original();
  }
   protected void hook27(){
    adler.reset();
    original();
  }
   protected void hook28(  int more){
    adler.update(inputBuf,inputOff,more);
    original(more);
  }
   protected void hook29(  byte[] buffer,  int offset,  int length){
    adler.update(buffer,offset,length);
    original(buffer,offset,length);
  }
}
