//

package net.sf.zipme;

/** 
 * This is the Deflater class.  The deflater class compresses input
 * with the deflate algorithm described in RFC 1951.  It has several
 * compression levels and three different strategies described below.
 * This class is <i>not</i> thread safe.  This is inherent in the API, due
 * to the split of deflate and setInput.
 * @author Jochen Hoenicke
 * @author Tom Tromey
 */
public class Deflater {
  /** 
 * The best and slowest compression level.  This tries to find very
 * long and distant string repetitions.  
 */
  public static final int BEST_COMPRESSION=9;
  /** 
 * The worst but fastest compression level.  
 */
  public static final int BEST_SPEED=1;
  /** 
 * The default compression level.
 */
  public static final int DEFAULT_COMPRESSION=-1;
  /** 
 * This level won't compress at all but output uncompressed blocks.
 */
  public static final int NO_COMPRESSION=0;
  /** 
 * The default strategy.
 */
  public static final int DEFAULT_STRATEGY=0;
  /** 
 * This strategy will only allow longer string repetitions.  It is
 * useful for random data with a small character set.
 */
  public static final int FILTERED=1;
  /** 
 * This strategy will not look for string repetitions at all.  It
 * only encodes with Huffman trees (which means, that more common
 * characters get a smaller encoding.  
 */
  public static final int HUFFMAN_ONLY=2;
  /** 
 * The compression method.  This is the only method supported so far.
 * There is no need to use this constant at all.
 */
  public static final int DEFLATED=8;
  public static final int IS_SETDICT=0x01;
  public static final int IS_FLUSHING=0x04;
  public static final int IS_FINISHING=0x08;
  private static final int INIT_STATE=0x00;
  private static final int SETDICT_STATE=0x01;
  private static final int INIT_FINISHING_STATE=0x08;
  private static final int SETDICT_FINISHING_STATE=0x09;
  public static final int BUSY_STATE=0x10;
  public static final int FLUSHING_STATE=0x14;
  public static final int FINISHING_STATE=0x1c;
  public static final int FINISHED_STATE=0x1e;
  public static final int CLOSED_STATE=0x7f;
  /** 
 * Compression level. 
 */
  public int level;
  /** 
 * should we include a header. 
 */
  public boolean noHeader;
  /** 
 * The current state. 
 */
  public int state;
  /** 
 * The total bytes of output written. 
 */
  public long totalOut;
  /** 
 * The pending output. 
 */
  public DeflaterPending pending;
  /** 
 * The deflater engine. 
 */
  public DeflaterEngine engine;
  /** 
 * Creates a new deflater with default compression level.
 */
  public Deflater(){
    this(DEFAULT_COMPRESSION,false);
  }
  /** 
 * Creates a new deflater with given compression level.
 * @param lvl the compression level, a value between NO_COMPRESSION
 * and BEST_COMPRESSION, or DEFAULT_COMPRESSION.  
 * @exception IllegalArgumentException if lvl is out of range.
 */
  public Deflater(  int lvl){
    this(lvl,false);
  }
  /** 
 * Creates a new deflater with given compression level.
 * @param lvl the compression level, a value between NO_COMPRESSION
 * and BEST_COMPRESSION.  
 * @param nowrap true, iff we should suppress the deflate header at the
 * beginning and the adler checksum at the end of the output.  This is
 * useful for the GZIP format.
 * @exception IllegalArgumentException if lvl is out of range.
 */
  public Deflater(  int lvl,  boolean nowrap){
    if (lvl == DEFAULT_COMPRESSION)     lvl=6;
 else     if (lvl < NO_COMPRESSION || lvl > BEST_COMPRESSION)     throw new IllegalArgumentException();
    pending=new DeflaterPending();
    this.hook25();
    this.noHeader=nowrap;
    this.hook24(lvl);
    reset();
  }
  /** 
 * Resets the deflater.  The deflater acts afterwards as if it was
 * just created with the same compression level and strategy as it
 * had before.  
 */
  public void reset(){
    state=(noHeader ? BUSY_STATE : INIT_STATE);
    totalOut=0;
    pending.reset();
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
    pending=null;
    state=CLOSED_STATE;
  }
  /** 
 * Gets the number of output bytes so far.
 */
  public int getTotalOut(){
    return (int)totalOut;
  }
  protected void hook24(  int lvl){
  }
  protected void hook25(){
  }
}
