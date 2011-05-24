package com.sleepycat.je.tree; 
import java.io.UnsupportedEncodingException; 
import java.nio.ByteBuffer; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.cleaner.FileSummary; 
import com.sleepycat.je.cleaner.PackedOffsets; 
import com.sleepycat.je.cleaner.TrackedFileSummary; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.log.LogException; 
import com.sleepycat.je.log.LogUtils; 
import com.sleepycat.je.log.LoggableObject; 
import de.ovgu.cide.jakutil.*; 
public final  class  FileSummaryLN  extends LN {
	 private static final String BEGIN_TAG="<fileSummaryLN>";

	 private static final String END_TAG="</fileSummaryLN>";

	 private FileSummary baseSummary;

	 private TrackedFileSummary trackedSummary;

	 private PackedOffsets obsoleteOffsets;

	 private boolean needOffsets;

	 private byte logVersion;

	 public FileSummaryLN( FileSummary baseSummary){ super(new byte[0]); assert baseSummary != null; this.baseSummary=baseSummary; obsoleteOffsets=new PackedOffsets(); logVersion=-1; }

	 public FileSummaryLN() throws DatabaseException { baseSummary=new FileSummary(); obsoleteOffsets=new PackedOffsets(); }

	 public void setTrackedSummary__wrappee__base( TrackedFileSummary trackedSummary){ this.trackedSummary=trackedSummary; needOffsets=true; }

	 public void setTrackedSummary( TrackedFileSummary trackedSummary){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setTrackedSummary__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public TrackedFileSummary getTrackedSummary__wrappee__base(){ return trackedSummary; }

	 public TrackedFileSummary getTrackedSummary(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTrackedSummary__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public FileSummary getBaseSummary__wrappee__base(){ return baseSummary; }

	 public FileSummary getBaseSummary(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBaseSummary__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public PackedOffsets getObsoleteOffsets__wrappee__base(){ return obsoleteOffsets; }

	 public PackedOffsets getObsoleteOffsets(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getObsoleteOffsets__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean hasStringKey__wrappee__base( byte[] bytes){ if (logVersion == 0 || bytes.length != 8) { return true; } else { return (bytes[4] >= '0' && bytes[4] <= '9'); } }

	 public boolean hasStringKey( byte[] bytes){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hasStringKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getFileNumber__wrappee__base( byte[] bytes){ if (hasStringKey(bytes)) { try { return Long.valueOf(new String(bytes,"UTF-8")).longValue(); } catch ( UnsupportedEncodingException shouldNeverHappen) { assert false : shouldNeverHappen; return 0; } } else { ByteBuffer buf=ByteBuffer.wrap(bytes); return LogUtils.readIntMSB(buf) & 0xFFFFFFFFL; } }

	 public long getFileNumber( byte[] bytes){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getFileNumber__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static byte\[\] makePartialKey__wrappee__base( long fileNum){ byte[] bytes=new byte[4]; ByteBuffer buf=ByteBuffer.wrap(bytes); LogUtils.writeIntMSB(buf,(int)fileNum); return bytes; }

	 public static byte[] makePartialKey( long fileNum){ t.in(Thread.currentThread().getStackTrace()[1].toString());	makePartialKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static byte\[\] makeFullKey__wrappee__base( long fileNum, int sequence){ assert sequence >= 0; byte[] bytes=new byte[8]; ByteBuffer buf=ByteBuffer.wrap(bytes); LogUtils.writeIntMSB(buf,(int)fileNum); LogUtils.writeIntMSB(buf,Integer.MAX_VALUE - sequence); return bytes; }

	 public static byte[] makeFullKey( long fileNum, int sequence){ t.in(Thread.currentThread().getStackTrace()[1].toString());	makeFullKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void postFetchInit__wrappee__base( DatabaseImpl db, long sourceLsn) throws DatabaseException { super.postFetchInit(db,sourceLsn); if (logVersion == 1 && db.getDbEnvironment().getUtilizationProfile().isRMWFixEnabled()) { obsoleteOffsets=new PackedOffsets(); } }

	 public void postFetchInit( DatabaseImpl db, long sourceLsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	postFetchInit__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ return dumpString(0,true); }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String beginTag__wrappee__base(){ return BEGIN_TAG; }

	 public String beginTag(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	beginTag__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String endTag__wrappee__base(){ return END_TAG; }

	 public String endTag(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	endTag__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String dumpString__wrappee__base( int nSpaces, boolean dumpTags){ StringBuffer sb=new StringBuffer(); sb.append(super.dumpString(nSpaces,dumpTags)); sb.append('\n'); if (!isDeleted()) { sb.append(baseSummary.toString()); sb.append(obsoleteOffsets.toString()); } return sb.toString(); }

	 public String dumpString( int nSpaces, boolean dumpTags){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void dumpLogAdditional__wrappee__base( StringBuffer sb, boolean verbose){ if (!isDeleted()) { baseSummary.dumpLog(sb,true); if (verbose) { obsoleteOffsets.dumpLog(sb,true); } } }

	 protected void dumpLogAdditional( StringBuffer sb, boolean verbose){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLogAdditional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected LogEntryType getTransactionalLogType__wrappee__base(){ assert false : "Txnl access to UP db not allowed"; return LogEntryType.LOG_FILESUMMARYLN; }

	 protected LogEntryType getTransactionalLogType(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTransactionalLogType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LogEntryType getLogType__wrappee__base(){ return LogEntryType.LOG_FILESUMMARYLN; }

	 public LogEntryType getLogType(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean marshallOutsideWriteLatch__wrappee__base(){ return false; }

	 public boolean marshallOutsideWriteLatch(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	marshallOutsideWriteLatch__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean countAsObsoleteWhenLogged__wrappee__base(){ return false; }

	 public boolean countAsObsoleteWhenLogged(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	countAsObsoleteWhenLogged__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getLogSize__wrappee__base(){ int size=super.getLogSize(); if (!isDeleted()) { size+=baseSummary.getLogSize(); getOffsets(); size+=obsoleteOffsets.getLogSize(); } return size; }

	 public int getLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void writeToLog__wrappee__base( ByteBuffer logBuffer){ if (trackedSummary != null) { baseSummary.add(trackedSummary); if (!isDeleted()) { getOffsets(); } trackedSummary.reset(); } super.writeToLog(logBuffer); if (!isDeleted()) { baseSummary.writeToLog(logBuffer); obsoleteOffsets.writeToLog(logBuffer); } }

	 public void writeToLog( ByteBuffer logBuffer){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeToLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void readFromLog__wrappee__base( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { super.readFromLog(itemBuffer,entryTypeVersion); logVersion=entryTypeVersion; if (!isDeleted()) { baseSummary.readFromLog(itemBuffer,entryTypeVersion); if (entryTypeVersion > 0) { obsoleteOffsets.readFromLog(itemBuffer,entryTypeVersion); } } }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readFromLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void getOffsets__wrappee__base(){ if (needOffsets) { long[] offsets=trackedSummary.getObsoleteOffsets(); if (offsets != null) { obsoleteOffsets.pack(offsets); } needOffsets=false; } }

	 private void getOffsets(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getOffsets__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
