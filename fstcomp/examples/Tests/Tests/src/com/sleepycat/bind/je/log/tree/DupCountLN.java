package com.sleepycat.je.tree; 
import java.nio.ByteBuffer; 
import com.sleepycat.je.dbi.MemoryBudget; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.log.LogException; 
import com.sleepycat.je.log.LogUtils; 
import de.ovgu.cide.jakutil.*; 
public final  class  DupCountLN  extends LN {
	 private static final String BEGIN_TAG="<dupCountLN>";

	 private static final String END_TAG="</dupCountLN>";

	 private int dupCount;

	 public DupCountLN( int count){ super(new byte[0]); this.dupCount=count; }

	 public DupCountLN(){ super(); dupCount=0; }

	 public int getDupCount(){ return dupCount; }

	 public int incDupCount(){ dupCount++; assert dupCount >= 0; return dupCount; }

	 public int decDupCount(){ dupCount--; assert dupCount >= 0; return dupCount; }

	 void setDupCount( int dupCount){ this.dupCount=dupCount; }

	 public boolean containsDuplicates(){ return true; }

	 public boolean isDeleted(){ return false; }

	 public String toString(){ return dumpString(0,true); }

	 public String beginTag(){ return BEGIN_TAG; }

	 public String endTag(){ return END_TAG; }

	 public String dumpString( int nSpaces, boolean dumpTags){ StringBuffer sb=new StringBuffer(); if (dumpTags) { sb.append(TreeUtils.indent(nSpaces)); sb.append(beginTag()); sb.append('\n'); } sb.append(TreeUtils.indent(nSpaces + 2)); sb.append("<count v=\"").append(dupCount).append("\"/>").append('\n'); sb.append(super.dumpString(nSpaces,false)); if (dumpTags) { sb.append(TreeUtils.indent(nSpaces)); sb.append(endTag()); } return sb.toString(); }

	 protected LogEntryType getTransactionalLogType(){ return LogEntryType.LOG_DUPCOUNTLN_TRANSACTIONAL; }

	 public LogEntryType getLogType(){ return LogEntryType.LOG_DUPCOUNTLN; }

	 public int getLogSize(){ return super.getLogSize() + LogUtils.INT_BYTES; }

	 public void writeToLog( ByteBuffer logBuffer){ super.writeToLog(logBuffer); LogUtils.writeInt(logBuffer,dupCount); }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { super.readFromLog(itemBuffer,entryTypeVersion); dupCount=LogUtils.readInt(itemBuffer); }

	 protected void dumpLogAdditional( StringBuffer sb, boolean verbose){ super.dumpLogAdditional(sb,verbose); sb.append("<count v=\"").append(dupCount).append("\"/>"); }


}
