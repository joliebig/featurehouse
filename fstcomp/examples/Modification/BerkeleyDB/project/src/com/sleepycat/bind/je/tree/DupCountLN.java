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

	 public int getDupCount__wrappee__base(){ return dupCount; }

	 public int getDupCount(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDupCount__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int incDupCount__wrappee__base(){ dupCount++; assert dupCount >= 0; return dupCount; }

	 public int incDupCount(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	incDupCount__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int decDupCount__wrappee__base(){ dupCount--; assert dupCount >= 0; return dupCount; }

	 public int decDupCount(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	decDupCount__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setDupCount__wrappee__base( int dupCount){ this.dupCount=dupCount; }

	 void setDupCount( int dupCount){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setDupCount__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean containsDuplicates__wrappee__base(){ return true; }

	 public boolean containsDuplicates(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	containsDuplicates__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isDeleted__wrappee__base(){ return false; }

	 public boolean isDeleted(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isDeleted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ return dumpString(0,true); }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String beginTag__wrappee__base(){ return BEGIN_TAG; }

	 public String beginTag(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	beginTag__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String endTag__wrappee__base(){ return END_TAG; }

	 public String endTag(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	endTag__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String dumpString__wrappee__base( int nSpaces, boolean dumpTags){ StringBuffer sb=new StringBuffer(); if (dumpTags) { sb.append(TreeUtils.indent(nSpaces)); sb.append(beginTag()); sb.append('\n'); } sb.append(TreeUtils.indent(nSpaces + 2)); sb.append("<count v=\"").append(dupCount).append("\"/>").append('\n'); sb.append(super.dumpString(nSpaces,false)); if (dumpTags) { sb.append(TreeUtils.indent(nSpaces)); sb.append(endTag()); } return sb.toString(); }

	 public String dumpString( int nSpaces, boolean dumpTags){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected LogEntryType getTransactionalLogType__wrappee__base(){ return LogEntryType.LOG_DUPCOUNTLN_TRANSACTIONAL; }

	 protected LogEntryType getTransactionalLogType(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTransactionalLogType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LogEntryType getLogType__wrappee__base(){ return LogEntryType.LOG_DUPCOUNTLN; }

	 public LogEntryType getLogType(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getLogSize__wrappee__base(){ return super.getLogSize() + LogUtils.INT_BYTES; }

	 public int getLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void writeToLog__wrappee__base( ByteBuffer logBuffer){ super.writeToLog(logBuffer); LogUtils.writeInt(logBuffer,dupCount); }

	 public void writeToLog( ByteBuffer logBuffer){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeToLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void readFromLog__wrappee__base( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { super.readFromLog(itemBuffer,entryTypeVersion); dupCount=LogUtils.readInt(itemBuffer); }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readFromLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void dumpLogAdditional__wrappee__base( StringBuffer sb, boolean verbose){ super.dumpLogAdditional(sb,verbose); sb.append("<count v=\"").append(dupCount).append("\"/>"); }

	 protected void dumpLogAdditional( StringBuffer sb, boolean verbose){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLogAdditional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
