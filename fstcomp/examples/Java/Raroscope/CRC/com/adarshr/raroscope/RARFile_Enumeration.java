////

/** 
 */
package com.adarshr.raroscope;
class RARFile_Enumeration {
   protected void hook29(  byte[] buf,  RAREntry entry) throws IOException {
    entry.setCrc(this.file.getLong(buf,16,19));
    original(buf,entry);
  }
}
