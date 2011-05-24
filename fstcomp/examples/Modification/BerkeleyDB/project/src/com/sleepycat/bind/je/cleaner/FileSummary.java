package com.sleepycat.je.cleaner; 
import java.nio.ByteBuffer; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.log.LogReadable; 
import com.sleepycat.je.log.LogUtils; 
import com.sleepycat.je.log.LogWritable; 
import de.ovgu.cide.jakutil.*; 
public  class  FileSummary  implements LogWritable, LogReadable {
	 public int totalCount;

	 public int totalSize;

	 public int totalINCount;

	 public int totalINSize;

	 public int totalLNCount;

	 public int totalLNSize;

	 public int obsoleteINCount;

	 public int obsoleteLNCount;

	 public FileSummary(){ }

	 public boolean isEmpty__wrappee__base(){ return totalCount == 0 && totalSize == 0 && obsoleteINCount == 0 && obsoleteLNCount == 0; }

	 public boolean isEmpty(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isEmpty__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getObsoleteLNSize__wrappee__base(){ if (totalLNCount == 0) { return 0; } long totalSize=totalLNSize; totalSize<<=8; long avgSizePerLN=totalSize / totalLNCount; return (int)((obsoleteLNCount * avgSizePerLN) >> 8); }

	 public int getObsoleteLNSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getObsoleteLNSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getObsoleteINSize__wrappee__base(){ if (totalINCount == 0) { return 0; } long totalSize=totalINSize; totalSize<<=8; long avgSizePerIN=totalSize / totalINCount; return (int)((obsoleteINCount * avgSizePerIN) >> 8); }

	 public int getObsoleteINSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getObsoleteINSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getObsoleteSize__wrappee__base() throws DatabaseException { if (totalSize > 0) { int leftoverSize=totalSize - (totalINSize + totalLNSize); int obsoleteSize=getObsoleteLNSize() + getObsoleteINSize() + leftoverSize; if (obsoleteSize > totalSize) { obsoleteSize=totalSize; } return obsoleteSize; } else { return 0; } }

	 public int getObsoleteSize() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getObsoleteSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getEntriesCounted__wrappee__base(){ return totalCount + obsoleteLNCount + obsoleteINCount; }

	 public int getEntriesCounted(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getEntriesCounted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getNonObsoleteCount__wrappee__base(){ return totalLNCount + totalINCount - obsoleteLNCount - obsoleteINCount; }

	 public int getNonObsoleteCount(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNonObsoleteCount__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void reset__wrappee__base(){ totalCount=0; totalSize=0; totalINCount=0; totalINSize=0; totalLNCount=0; totalLNSize=0; obsoleteINCount=0; obsoleteLNCount=0; }

	 public void reset(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	reset__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void add__wrappee__base( FileSummary o){ totalCount+=o.totalCount; totalSize+=o.totalSize; totalINCount+=o.totalINCount; totalINSize+=o.totalINSize; totalLNCount+=o.totalLNCount; totalLNSize+=o.totalLNSize; obsoleteINCount+=o.obsoleteINCount; obsoleteLNCount+=o.obsoleteLNCount; }

	 public void add( FileSummary o){ t.in(Thread.currentThread().getStackTrace()[1].toString());	add__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getLogSize__wrappee__base(){ return 8 * LogUtils.getIntLogSize(); }

	 public int getLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void writeToLog__wrappee__base( ByteBuffer buf){ LogUtils.writeInt(buf,totalCount); LogUtils.writeInt(buf,totalSize); LogUtils.writeInt(buf,totalINCount); LogUtils.writeInt(buf,totalINSize); LogUtils.writeInt(buf,totalLNCount); LogUtils.writeInt(buf,totalLNSize); LogUtils.writeInt(buf,obsoleteINCount); LogUtils.writeInt(buf,obsoleteLNCount); }

	 public void writeToLog( ByteBuffer buf){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeToLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void readFromLog__wrappee__base( ByteBuffer buf, byte entryTypeVersion){ totalCount=LogUtils.readInt(buf); totalSize=LogUtils.readInt(buf); totalINCount=LogUtils.readInt(buf); totalINSize=LogUtils.readInt(buf); totalLNCount=LogUtils.readInt(buf); totalLNSize=LogUtils.readInt(buf); obsoleteINCount=LogUtils.readInt(buf); if (obsoleteINCount == -1) { obsoleteINCount=totalINCount; } obsoleteLNCount=LogUtils.readInt(buf); }

	 public void readFromLog( ByteBuffer buf, byte entryTypeVersion){ t.in(Thread.currentThread().getStackTrace()[1].toString());	readFromLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void dumpLog__wrappee__base( StringBuffer buf, boolean verbose){ buf.append("<summary totalCount=\""); buf.append(totalCount); buf.append("\" totalSize=\""); buf.append(totalSize); buf.append("\" totalINCount=\""); buf.append(totalINCount); buf.append("\" totalINSize=\""); buf.append(totalINSize); buf.append("\" totalLNCount=\""); buf.append(totalLNCount); buf.append("\" totalLNSize=\""); buf.append(totalLNSize); buf.append("\" obsoleteINCount=\""); buf.append(obsoleteINCount); buf.append("\" obsoleteLNCount=\""); buf.append(obsoleteLNCount); buf.append("\"/>"); }

	 public void dumpLog( StringBuffer buf, boolean verbose){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getTransactionId__wrappee__base(){ return -1; }

	 public long getTransactionId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTransactionId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean logEntryIsTransactional__wrappee__base(){ return false; }

	 public boolean logEntryIsTransactional(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	logEntryIsTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ StringBuffer buf=new StringBuffer(); dumpLog(buf,true); return buf.toString(); }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
