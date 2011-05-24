////

/** 
 */
package com.adarshr.raroscope;
class RARFile_Enumeration {
   protected void hook30(  byte[] buf,  RAREntry entry) throws IOException {
    entry.setVersion(this.file.toVersion(buf[24] & 0xFF));
    original(buf,entry);
  }
}
