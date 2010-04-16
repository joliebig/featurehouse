////

package com.adarshr.raroscope;
import java.util.Date;

/** 
 * Represents an entry in a RAR archive.
 * @author Adarsh Ramamurthy
 * @version 1.0, 10th March 2008
 */
public class RAREntry {
  private String name;
  private Date time;
  private long size;
  private long compressedSize;
  private boolean directory;
  private String method;
  public void setDirectory(  boolean directory){
    this.directory=directory;
  }
  public void setName(  String name){
    this.name=name;
  }
  public void setTime(  Date time){
    this.time=time;
  }
  public void setSize(  long size){
    this.size=size;
  }
  public void setCompressedSize(  long compressedSize){
    this.compressedSize=compressedSize;
  }
  public String getName(){
    return name;
  }
  public Date getTime(){
    return time;
  }
  public long getSize(){
    return size;
  }
  public long getCompressedSize(){
    return compressedSize;
  }
  public boolean isDirectory(){
    return directory;
  }
  public String getMethod(){
    return method;
  }
  public void setMethod(  String method){
    this.method=method;
  }
  /** 
 * Constructs a <code>String</code> with all attributes
 * in name = value format.
 * @return a <code>String</code> representation 
 * of this object.
 */
  public String toString(){
    final String TAB="    ";
    String retValue="";
    String t3="";
    t3=this.hook26(TAB,t3);
    String localCRC=t3;
    String t2="";
    t2=this.hook25(TAB,t2);
    String localOS=t2;
    String t="";
    t=this.hook27(TAB,t);
    String localVersion=t;
    retValue="RAREntry ( " + "name = " + this.name + TAB+ "time = "+ this.time+ TAB+ "size = "+ this.size+ TAB+ "compressedSize = "+ this.compressedSize+ TAB+ localCRC+ "directory = "+ this.directory+ TAB+ localOS+ "method = "+ this.method+ TAB+ localVersion+ " )";
    return retValue;
  }
  protected String hook25(  String TAB,  String t2){
    return t2;
  }
  protected String hook26(  String TAB,  String t3){
    return t3;
  }
  protected String hook27(  String TAB,  String t){
    return t;
  }
}
