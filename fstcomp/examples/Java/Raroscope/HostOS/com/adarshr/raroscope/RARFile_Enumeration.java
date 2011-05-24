////

/** 
 */
package com.adarshr.raroscope;
class RARFile_Enumeration {
   protected void hook28(  byte[] buf,  RAREntry entry) throws IOException {
    entry.setHostOS(this.file.toOS(buf[15] & 0xFF));
    original(buf,entry);
  }
}
