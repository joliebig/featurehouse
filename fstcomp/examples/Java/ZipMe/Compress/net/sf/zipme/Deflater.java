//

package net.sf.zipme;
class Deflater {
  /** 
 * Gets the number of input bytes processed so far.
 */
   public int getTotalIn(){
    return (int)engine.getTotalIn();
  }
  /** 
 * Gets the number of input bytes processed so far.
 * @since 1.5
 */
   public long getBytesRead(){
    return engine.getTotalIn();
  }
  /** 
 * Gets the number of output bytes so far.
 * @since 1.5
 */
   public long getBytesWritten(){
    return totalOut;
  }
  /** 
 * Flushes the current input block.  Further calls to deflate() will
 * produce enough output to inflate everything in the current input
 * block.  This is not part of Sun's JDK so I have made it package
 * private.  It is used by DeflaterOutputStream to implement
 * flush().
 */
   void flush(){
    state|=IS_FLUSHING;
  }
  /** 
 * Finishes the deflater with the current input block.  It is an error
 * to give more input after this method was called.  This method must
 * be called to force all bytes to be flushed.
 */
   public void finish(){
    state|=IS_FLUSHING | IS_FINISHING;
  }
  /** 
 * Returns true iff the stream was finished and no more output bytes
 * are available.
 */
   public boolean finished(){
    return state == FINISHED_STATE && pending.isFlushed();
  }
  /** 
 * Returns true, if the input buffer is empty.
 * You should then call setInput(). <br>
 * <em>NOTE</em>: This method can also return true when the stream
 * was finished.  
 */
   public boolean needsInput(){
    return engine.needsInput();
  }
  /** 
 * Sets the data which should be compressed next.  This should be only
 * called when needsInput indicates that more input is needed.
 * If you call setInput when needsInput() returns false, the
 * previous input that is still pending will be thrown away.
 * The given byte array should not be changed, before needsInput() returns
 * true again.
 * This call is equivalent to <code>setInput(input, 0, input.length)</code>.
 * @param input the buffer containing the input data.
 * @exception IllegalStateException if the buffer was finished() or ended().
 */
   public void setInput(  byte[] input){
    setInput(input,0,input.length);
  }
  /** 
 * Sets the data which should be compressed next.  This should be
 * only called when needsInput indicates that more input is needed.
 * The given byte array should not be changed, before needsInput() returns
 * true again.
 * @param input the buffer containing the input data.
 * @param off the start of the data.
 * @param len the length of the data.  
 * @exception IllegalStateException if the buffer was finished() or ended()
 * or if previous input is still pending.
 */
   public void setInput(  byte[] input,  int off,  int len){
    if ((state & IS_FINISHING) != 0)     throw new IllegalStateException("finish()/end() already called");
    engine.setInput(input,off,len);
  }
  /** 
 * Sets the compression level.  There is no guarantee of the exact
 * position of the change, but if you call this when needsInput is
 * true the change of compression level will occur somewhere near
 * before the end of the so far given input.  
 * @param lvl the new compression level.
 */
   public void setLevel(  int lvl){
    if (lvl == DEFAULT_COMPRESSION)     lvl=6;
 else     if (lvl < NO_COMPRESSION || lvl > BEST_COMPRESSION)     throw new IllegalArgumentException();
    if (level != lvl) {
      level=lvl;
      engine.setLevel(lvl);
    }
  }
  /** 
 * Sets the compression strategy. Strategy is one of
 * DEFAULT_STRATEGY, HUFFMAN_ONLY and FILTERED.  For the exact
 * position where the strategy is changed, the same as for
 * setLevel() applies.
 * @param stgy the new compression strategy.
 */
   public void setStrategy(  int stgy){
    if (stgy != DEFAULT_STRATEGY && stgy != FILTERED && stgy != HUFFMAN_ONLY)     throw new IllegalArgumentException();
    engine.setStrategy(stgy);
  }
  /** 
 * Deflates the current input block to the given array.  It returns 
 * the number of bytes compressed, or 0 if either 
 * needsInput() or finished() returns true or length is zero.
 * @param output the buffer where to write the compressed data.
 */
   public int deflate(  byte[] output){
    return deflate(output,0,output.length);
  }
  /** 
 * Deflates the current input block to the given array.  It returns 
 * the number of bytes compressed, or 0 if either 
 * needsInput() or finished() returns true or length is zero.
 * @param output the buffer where to write the compressed data.
 * @param offset the offset into the output array.
 * @param length the maximum number of bytes that may be written.
 * @exception IllegalStateException if end() was called.
 * @exception IndexOutOfBoundsException if offset and/or length
 * don't match the array length.  
 */
   public int deflate(  byte[] output,  int offset,  int length){
    return new Deflater_deflate2(this,output,offset,length).execute();
  }
  /** 
 * Sets the dictionary which should be used in the deflate process.
 * This call is equivalent to <code>setDictionary(dict, 0,
 * dict.length)</code>.  
 * @param dict the dictionary.  
 * @exception IllegalStateException if setInput () or deflate ()
 * were already called or another dictionary was already set.  
 */
   public void setDictionary(  byte[] dict){
    setDictionary(dict,0,dict.length);
  }
  /** 
 * Sets the dictionary which should be used in the deflate process.
 * The dictionary should be a byte array containing strings that are
 * likely to occur in the data which should be compressed.  The
 * dictionary is not stored in the compressed output, only a
 * checksum.  To decompress the output you need to supply the same
 * dictionary again.
 * @param dict the dictionary.
 * @param offset an offset into the dictionary.
 * @param length the length of the dictionary.
 * @exception IllegalStateException if setInput () or deflate () were
 * already called or another dictionary was already set.
 */
   public void setDictionary(  byte[] dict,  int offset,  int length){
    if (state != INIT_STATE)     throw new IllegalStateException();
    state=SETDICT_STATE;
    engine.setDictionary(dict,offset,length);
  }
   protected void hook24(  int lvl){
    setStrategy(DEFAULT_STRATEGY);
    setLevel(lvl);
    original(lvl);
  }
   protected void hook25(){
    engine=new DeflaterEngine(pending);
    original();
  }
  /** 
 * Resets the deflater.  The deflater acts afterwards as if it was
 * just created with the same compression level and strategy as it
 * had before.  
 */
   public void reset(){
    original();
    engine.reset();
  }
  /** 
 * Frees all objects allocated by the compressor.  There's no
 * reason to call this, since you can just rely on garbage
 * collection.  Exists only for compatibility against Sun's JDK,
 * where the compressor allocates native memory.
 * If you call any method (even reset) afterwards the behaviour is
 * <i>undefined</i>.  
 */
   public void end(){
    engine=null;
    original();
  }
}
