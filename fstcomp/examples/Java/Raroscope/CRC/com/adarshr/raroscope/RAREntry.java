////

package com.adarshr.raroscope;
class RAREntry {
  private long crc;
   public void setCrc(  long crc){
    this.crc=crc;
  }
   public long getCrc(){
    return crc;
  }
   protected String hook26(  String TAB,  String t3){
    t3+="crc = " + this.crc + TAB;
    return original(TAB,t3);
  }
}
