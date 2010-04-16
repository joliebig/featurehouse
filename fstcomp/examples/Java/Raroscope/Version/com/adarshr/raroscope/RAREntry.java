////

package com.adarshr.raroscope;
class RAREntry {
  private String version;
   public String getVersion(){
    return version;
  }
   public void setVersion(  String version){
    this.version=version;
  }
   protected String hook27(  String TAB,  String t){
    t+="version = " + this.version + TAB;
    return original(TAB,t);
  }
}
