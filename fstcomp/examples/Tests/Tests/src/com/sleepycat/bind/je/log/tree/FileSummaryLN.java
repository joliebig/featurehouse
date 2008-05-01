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

	 public void setTrackedSummary( TrackedFileSummary trackedSummary){ this.trackedSummary=trackedSummary; needOffsets=true; }

	 public TrackedFileSummary getTrackedSummary(){ return trackedSummary; }

	 public FileSummary getBaseSummary(){ return baseSummary; }

	 public PackedOffsets getObsoleteOffsets(){ return obsoleteOffsets; }

	 public boolean hasStringKey( byte[] bytes){ if (logVersion == 0 || bytes.length != 8) { return true; } else { return (bytes[4] >= '0' && bytes[4] <= '9'); } }

	 public long getFileNumber( byte[] bytes){ if (hasStringKey(bytes)) { try { return Long.valueOf(new String(bytes,"UTF-8")).longValue(); } catch ( UnsupportedEncodingException shouldNeverHappen) { assert false : shouldNeverHappen; return 0; } } else { ByteBuffer buf=ByteBuffer.wrap(bytes); return LogUtils.readIntMSB(buf) & 0xFFFFFFFFL; } }

	 public static byte[] makePartialKey( long fileNum){ byte[] bytes=new byte[4]; ByteBuffer buf=ByteBuffer.wrap(bytes); LogUtils.writeIntMSB(buf,(int)fileNum); return bytes; }

	 public static byte[] makeFullKey( long fileNum, int sequence){ assert sequence >= 0; byte[] bytes=new byte[8]; ByteBuffer buf=ByteBuffer.wrap(bytes); LogUtils.writeIntMSB(buf,(int)fileNum); LogUtils.writeIntMSB(buf,Integer.MAX_VALUE - sequence); return bytes; }

	 public void postFetchInit( DatabaseImpl db, long sourceLsn) throws DatabaseException { super.postFetchInit(db,sourceLsn); if (logVersion == 1 && db.getDbEnvironment().getUtilizationProfile().isRMWFixEnabled()) { obsoleteOffsets=new PackedOffsets(); } }

	 public String toString(){ return dumpString(0,true); }

	 public String beginTag(){ return BEGIN_TAG; }

	 public String endTag(){ return END_TAG; }

	 public String dumpString( int nSpaces, boolean dumpTags){ StringBuffer sb=new StringBuffer(); sb.append(super.dumpString(nSpaces,dumpTags)); sb.append('\n'); if (!isDeleted()) { sb.append(baseSummary.toString()); sb.append(obsoleteOffsets.toString()); } return sb.toString(); }

	 protected void dumpLogAdditional( StringBuffer sb, boolean verbose){ if (!isDeleted()) { baseSummary.dumpLog(sb,true); if (verbose) { obsoleteOffsets.dumpLog(sb,true); } } }

	 protected LogEntryType getTransactionalLogType(){ assert false : "Txnl access to UP db not allowed"; return LogEntryType.LOG_FILESUMMARYLN; }

	 public LogEntryType getLogType(){ return LogEntryType.LOG_FILESUMMARYLN; }

	 public boolean marshallOutsideWriteLatch(){ return false; }

	 public boolean countAsObsoleteWhenLogged(){ return false; }

	 public int getLogSize(){ int size=super.getLogSize(); if (!isDeleted()) { size+=baseSummary.getLogSize(); getOffsets(); size+=obsoleteOffsets.getLogSize(); } return size; }

	 public void writeToLog( ByteBuffer logBuffer){ if (trackedSummary != null) { baseSummary.add(trackedSummary); if (!isDeleted()) { getOffsets(); } trackedSummary.reset(); } super.writeToLog(logBuffer); if (!isDeleted()) { baseSummary.writeToLog(logBuffer); obsoleteOffsets.writeToLog(logBuffer); } }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { super.readFromLog(itemBuffer,entryTypeVersion); logVersion=entryTypeVersion; if (!isDeleted()) { baseSummary.readFromLog(itemBuffer,entryTypeVersion); if (entryTypeVersion > 0) { obsoleteOffsets.readFromLog(itemBuffer,entryTypeVersion); } } }

	 private void getOffsets(){ if (needOffsets) { long[] offsets=trackedSummary.getObsoleteOffsets(); if (offsets != null) { obsoleteOffsets.pack(offsets); } needOffsets=false; } }


}
