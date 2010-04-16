//

package net.sf.zipme;
class Inflater {
  private Adler32 adler;
  /** 
 * Gets the adler checksum.  This is either the checksum of all
 * uncompressed bytes returned by inflate(), or if needsDictionary()
 * returns true (and thus no output was yet produced) this is the
 * adler checksum of the expected dictionary.
 * @return the adler checksum.
 */
   public int getAdler(){
    return needsDictionary() ? readAdler : (int)adler.getValue();
  }
   protected void hook32(){
    this.adler=new Adler32();
    original();
  }
  /** 
 * Frees all objects allocated by the inflater.  There's no reason
 * to call this, since you can just rely on garbage collection (even
 * for the Sun implementation).  Exists only for compatibility
 * with Sun's JDK, where the compressor allocates native memory.
 * If you call any method (even reset) afterwards the behaviour is
 * <i>undefined</i>.  
 */
   public void end(){
    original();
    adler=null;
  }
   protected void hook33(  byte[] buf,  int off,  int more) throws DataFormatException {
    adler.update(buf,off,more);
    original(buf,off,more);
  }
  /** 
 * Resets the inflater so that a new stream can be decompressed.  All
 * pending input and output will be discarded.
 */
   public void reset(){
    original();
    adler.reset();
  }
   protected void hook34(  byte[] buffer,  int off,  int len){
    adler.update(buffer,off,len);
    if ((int)adler.getValue() != readAdler)     throw new IllegalArgumentException("Wrong adler checksum");
    adler.reset();
    original(buffer,off,len);
  }
   protected void hook35() throws DataFormatException {
    if ((int)adler.getValue() != readAdler)     throw new DataFormatException("Adler chksum doesn't match: " + Integer.toHexString((int)adler.getValue()) + " vs. "+ Integer.toHexString(readAdler));
    original();
  }
}
