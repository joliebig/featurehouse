////

package com.adarshr.raroscope;
class RAREntry {
  private String hostOS;
   public String getHostOS(){
    return hostOS;
  }
   public void setHostOS(  String hostOS){
    this.hostOS=hostOS;
  }
   protected String hook25(  String TAB,  String t2){
    t2+="hostOS = " + this.hostOS + TAB;
    return original(TAB,t2);
  }
}
