class RARFile{
	
  /** 
 * Enumerates all the entries in the RAR archive.
 * @return an instance of <tt>Enumeration</tt> for displaying the entries.
 * @see RAREntry
 */
  public Enumeration entries(){
    return new RARFile_Enumeration(this);
  }
}