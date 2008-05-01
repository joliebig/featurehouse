package com.sleepycat.je.log; 
import java.nio.ByteBuffer; 
import java.sql.Timestamp; 
import java.util.Calendar; 
import com.sleepycat.je.DatabaseException; 
import de.ovgu.cide.jakutil.*; 
public  class  FileHeader  implements LoggableObject, LogReadable {
	 private static final int LOG_VERSION=3;

	 private long fileNum;

	 private long lastEntryInPrevFileOffset;

	 private Timestamp time;

	 private int logVersion;

	 FileHeader( long fileNum, long lastEntryInPrevFileOffset){ this.fileNum=fileNum; this.lastEntryInPrevFileOffset=lastEntryInPrevFileOffset; Calendar now=Calendar.getInstance(); time=new Timestamp(now.getTimeInMillis()); logVersion=LOG_VERSION; }

	 public FileHeader(){ }

	 boolean validate( String fileName, long expectedFileNum) throws DatabaseException { if (fileNum != expectedFileNum) { throw new LogException("Wrong filenum in header for file " + fileName + " expected "+ expectedFileNum+ " got "+ fileNum); } return logVersion < LOG_VERSION; }

	 long getLastEntryInPrevFileOffset(){ return lastEntryInPrevFileOffset; }

	 public LogEntryType getLogType(){ return LogEntryType.LOG_FILE_HEADER; }

	 public boolean marshallOutsideWriteLatch(){ return true; }

	 public boolean countAsObsoleteWhenLogged(){ return false; }

	 public void postLogWork( long justLoggedLsn) throws DatabaseException { }

	 static int entrySize(){ return LogUtils.getTimestampLogSize() + LogUtils.UNSIGNED_INT_BYTES + LogUtils.LONG_BYTES+ LogUtils.INT_BYTES; }

	 public int getLogSize(){ return entrySize(); }

	 public void writeToLog( ByteBuffer logBuffer){ LogUtils.writeTimestamp(logBuffer,time); LogUtils.writeUnsignedInt(logBuffer,fileNum); LogUtils.writeLong(logBuffer,lastEntryInPrevFileOffset); LogUtils.writeInt(logBuffer,logVersion); }

	 public void readFromLog( ByteBuffer logBuffer, byte entryTypeVersion) throws LogException { time=LogUtils.readTimestamp(logBuffer); fileNum=LogUtils.getUnsignedInt(logBuffer); lastEntryInPrevFileOffset=LogUtils.readLong(logBuffer); logVersion=LogUtils.readInt(logBuffer); if (logVersion > LOG_VERSION) { throw new LogException("Expected log version " + LOG_VERSION + " or earlier but found "+ logVersion+ " -- this version is not supported."); } }

	 public void dumpLog( StringBuffer sb, boolean verbose){ sb.append("<FileHeader num=\"0x"); sb.append(Long.toHexString(fileNum)); sb.append("\" lastEntryInPrevFileOffset=\"0x"); sb.append(Long.toHexString(lastEntryInPrevFileOffset)); sb.append("\" logVersion=\"0x"); sb.append(Integer.toHexString(logVersion)); sb.append("\" time=\"").append(time); sb.append("\"/>"); }

	 public boolean logEntryIsTransactional(){ return false; }

	 public long getTransactionId(){ return 0; }

	 public String toString(){ StringBuffer sb=new StringBuffer(); dumpLog(sb,true); return sb.toString(); }


}
