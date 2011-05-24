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

	 boolean validate__wrappee__base( String fileName, long expectedFileNum) throws DatabaseException { if (fileNum != expectedFileNum) { throw new LogException("Wrong filenum in header for file " + fileName + " expected "+ expectedFileNum+ " got "+ fileNum); } return logVersion < LOG_VERSION; }

	 boolean validate( String fileName, long expectedFileNum) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	validate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 long getLastEntryInPrevFileOffset__wrappee__base(){ return lastEntryInPrevFileOffset; }

	 long getLastEntryInPrevFileOffset(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLastEntryInPrevFileOffset__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LogEntryType getLogType__wrappee__base(){ return LogEntryType.LOG_FILE_HEADER; }

	 public LogEntryType getLogType(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean marshallOutsideWriteLatch__wrappee__base(){ return true; }

	 public boolean marshallOutsideWriteLatch(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	marshallOutsideWriteLatch__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean countAsObsoleteWhenLogged__wrappee__base(){ return false; }

	 public boolean countAsObsoleteWhenLogged(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	countAsObsoleteWhenLogged__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void postLogWork__wrappee__base( long justLoggedLsn) throws DatabaseException { }

	 public void postLogWork( long justLoggedLsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	postLogWork__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 static int entrySize__wrappee__base(){ return LogUtils.getTimestampLogSize() + LogUtils.UNSIGNED_INT_BYTES + LogUtils.LONG_BYTES+ LogUtils.INT_BYTES; }

	 static int entrySize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entrySize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getLogSize__wrappee__base(){ return entrySize(); }

	 public int getLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void writeToLog__wrappee__base( ByteBuffer logBuffer){ LogUtils.writeTimestamp(logBuffer,time); LogUtils.writeUnsignedInt(logBuffer,fileNum); LogUtils.writeLong(logBuffer,lastEntryInPrevFileOffset); LogUtils.writeInt(logBuffer,logVersion); }

	 public void writeToLog( ByteBuffer logBuffer){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeToLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void readFromLog__wrappee__base( ByteBuffer logBuffer, byte entryTypeVersion) throws LogException { time=LogUtils.readTimestamp(logBuffer); fileNum=LogUtils.getUnsignedInt(logBuffer); lastEntryInPrevFileOffset=LogUtils.readLong(logBuffer); logVersion=LogUtils.readInt(logBuffer); if (logVersion > LOG_VERSION) { throw new LogException("Expected log version " + LOG_VERSION + " or earlier but found "+ logVersion+ " -- this version is not supported."); } }

	 public void readFromLog( ByteBuffer logBuffer, byte entryTypeVersion) throws LogException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readFromLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void dumpLog__wrappee__base( StringBuffer sb, boolean verbose){ sb.append("<FileHeader num=\"0x"); sb.append(Long.toHexString(fileNum)); sb.append("\" lastEntryInPrevFileOffset=\"0x"); sb.append(Long.toHexString(lastEntryInPrevFileOffset)); sb.append("\" logVersion=\"0x"); sb.append(Integer.toHexString(logVersion)); sb.append("\" time=\"").append(time); sb.append("\"/>"); }

	 public void dumpLog( StringBuffer sb, boolean verbose){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean logEntryIsTransactional__wrappee__base(){ return false; }

	 public boolean logEntryIsTransactional(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	logEntryIsTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getTransactionId__wrappee__base(){ return 0; }

	 public long getTransactionId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTransactionId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ StringBuffer sb=new StringBuffer(); dumpLog(sb,true); return sb.toString(); }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
