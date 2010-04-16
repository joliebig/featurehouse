//

package net.sf.zipme;

/** 
 * Interface to compute a data checksum used by checked input/output streams.
 * A data checksum can be updated by one byte or with a byte array. After each
 * update the value of the current checksum can be returned by calling
 * <code>getValue</code>. The complete checksum object can also be reset
 * so it can be used again with new data.
 * @see CheckedInputStream
 * @see CheckedOutputStream
 * @author Per Bothner
 * @author Jochen Hoenicke
 */
public interface Checksum {
  /** 
 * Returns the data checksum computed so far.
 */
  long getValue();
  /** 
 * Resets the data checksum as if no update was ever called.
 */
  void reset();
  /** 
 * Adds one byte to the data checksum.
 * @param bval the data value to add. The high byte of the int is ignored.
 */
  void update(  int bval);
  /** 
 * Adds the byte array to the data checksum.
 * @param buf the buffer which contains the data
 * @param off the offset in the buffer where the data starts
 * @param len the length of the data
 */
  void update(  byte[] buf,  int off,  int len);
}
