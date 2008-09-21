package com.sleepycat.je.recovery; 
import java.nio.ByteBuffer; 
import java.sql.Timestamp; 
import java.util.Calendar; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.log.LogException; 
import com.sleepycat.je.log.LogReadable; 
import com.sleepycat.je.log.LogUtils; 
import com.sleepycat.je.log.LoggableObject; 
import com.sleepycat.je.utilint.DbLsn; 
import de.ovgu.cide.jakutil.*; 
public  class  CheckpointEnd  implements LoggableObject, LogReadable {
	 private String invoker;

	 private Timestamp endTime;

	 private long checkpointStartLsn;

	 private boolean rootLsnExists;

	 private long rootLsn;

	 private long firstActiveLsn;

	 private long lastNodeId;

	 private int lastDbId;

	 private long lastTxnId;

	 private long id;

	 public CheckpointEnd( String invoker, long checkpointStartLsn, long rootLsn, long firstActiveLsn, long lastNodeId, int lastDbId, long lastTxnId, long id){ if (invoker == null) { this.invoker=""; } else { this.invoker=invoker; } Calendar cal=Calendar.getInstance(); this.endTime=new Timestamp(cal.getTime().getTime()); this.checkpointStartLsn=checkpointStartLsn; this.rootLsn=rootLsn; if (rootLsn == DbLsn.NULL_LSN) { rootLsnExists=false; } else { rootLsnExists=true; } if (firstActiveLsn == DbLsn.NULL_LSN) { this.firstActiveLsn=checkpointStartLsn; } else { this.firstActiveLsn=firstActiveLsn; } this.lastNodeId=lastNodeId; this.lastDbId=lastDbId; this.lastTxnId=lastTxnId; this.id=id; }

	 public CheckpointEnd(){ checkpointStartLsn=DbLsn.NULL_LSN; rootLsn=DbLsn.NULL_LSN; firstActiveLsn=DbLsn.NULL_LSN; }

	 public LogEntryType getLogType__wrappee__base(){ return LogEntryType.LOG_CKPT_END; }

	 public LogEntryType getLogType(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean marshallOutsideWriteLatch__wrappee__base(){ return true; }

	 public boolean marshallOutsideWriteLatch(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	marshallOutsideWriteLatch__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean countAsObsoleteWhenLogged__wrappee__base(){ return false; }

	 public boolean countAsObsoleteWhenLogged(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	countAsObsoleteWhenLogged__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void postLogWork__wrappee__base( long justLoggedLsn){ }

	 public void postLogWork( long justLoggedLsn){ t.in(Thread.currentThread().getStackTrace()[1].toString());	postLogWork__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getLogSize__wrappee__base(){ int size=LogUtils.getStringLogSize(invoker) + LogUtils.getTimestampLogSize() + LogUtils.getLongLogSize()+ LogUtils.getBooleanLogSize()+ LogUtils.getLongLogSize()+ LogUtils.getLongLogSize()+ LogUtils.getIntLogSize()+ LogUtils.getLongLogSize()+ LogUtils.getLongLogSize(); if (rootLsnExists) { size+=LogUtils.getLongLogSize(); } return size; }

	 public int getLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void writeToLog__wrappee__base( ByteBuffer logBuffer){ LogUtils.writeString(logBuffer,invoker); LogUtils.writeTimestamp(logBuffer,endTime); LogUtils.writeLong(logBuffer,checkpointStartLsn); LogUtils.writeBoolean(logBuffer,rootLsnExists); if (rootLsnExists) { LogUtils.writeLong(logBuffer,rootLsn); } LogUtils.writeLong(logBuffer,firstActiveLsn); LogUtils.writeLong(logBuffer,lastNodeId); LogUtils.writeInt(logBuffer,lastDbId); LogUtils.writeLong(logBuffer,lastTxnId); LogUtils.writeLong(logBuffer,id); }

	 public void writeToLog( ByteBuffer logBuffer){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeToLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void readFromLog__wrappee__base( ByteBuffer logBuffer, byte entryTypeVersion) throws LogException { invoker=LogUtils.readString(logBuffer); endTime=LogUtils.readTimestamp(logBuffer); checkpointStartLsn=LogUtils.readLong(logBuffer); rootLsnExists=LogUtils.readBoolean(logBuffer); if (rootLsnExists) { rootLsn=LogUtils.readLong(logBuffer); } firstActiveLsn=LogUtils.readLong(logBuffer); lastNodeId=LogUtils.readLong(logBuffer); lastDbId=LogUtils.readInt(logBuffer); lastTxnId=LogUtils.readLong(logBuffer); id=LogUtils.readLong(logBuffer); }

	 public void readFromLog( ByteBuffer logBuffer, byte entryTypeVersion) throws LogException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readFromLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void dumpLog__wrappee__base( StringBuffer sb, boolean verbose){ sb.append("<CkptEnd invoker=\"").append(invoker); sb.append("\" time=\"").append(endTime); sb.append("\" lastNodeId=\"").append(lastNodeId); sb.append("\" lastDbId=\"").append(lastDbId); sb.append("\" lastTxnId=\"").append(lastTxnId); sb.append("\" id=\"").append(id); sb.append("\" rootExists=\"").append(rootLsnExists); sb.append("\">"); sb.append("<ckptStart>"); sb.append(DbLsn.toString(checkpointStartLsn)); sb.append("</ckptStart>"); if (rootLsnExists) { sb.append("<root>"); sb.append(DbLsn.toString(rootLsn)); sb.append("</root>"); } sb.append("<firstActive>"); sb.append(DbLsn.toString(firstActiveLsn)); sb.append("</firstActive>"); sb.append("</CkptEnd>"); }

	 public void dumpLog( StringBuffer sb, boolean verbose){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean logEntryIsTransactional__wrappee__base(){ return false; }

	 public boolean logEntryIsTransactional(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	logEntryIsTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getTransactionId__wrappee__base(){ return 0; }

	 public long getTransactionId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTransactionId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ StringBuffer sb=new StringBuffer(); sb.append("time=").append(endTime); sb.append(" lastNodeId=").append(lastNodeId); sb.append(" lastDbId=").append(lastDbId); sb.append(" lastTxnId=").append(lastTxnId); sb.append(" id=").append(id); sb.append(" rootExists=").append(rootLsnExists); sb.append(" ckptStartLsn=").append(DbLsn.getNoFormatString(checkpointStartLsn)); if (rootLsnExists) { sb.append(" root=").append(DbLsn.getNoFormatString(rootLsn)); } sb.append(" firstActive=").append(DbLsn.getNoFormatString(firstActiveLsn)); return sb.toString(); }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 long getCheckpointStartLsn__wrappee__base(){ return checkpointStartLsn; }

	 long getCheckpointStartLsn(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getCheckpointStartLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 long getRootLsn__wrappee__base(){ return rootLsn; }

	 long getRootLsn(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getRootLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 long getFirstActiveLsn__wrappee__base(){ return firstActiveLsn; }

	 long getFirstActiveLsn(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getFirstActiveLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 long getLastNodeId__wrappee__base(){ return lastNodeId; }

	 long getLastNodeId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLastNodeId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 int getLastDbId__wrappee__base(){ return lastDbId; }

	 int getLastDbId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLastDbId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 long getLastTxnId__wrappee__base(){ return lastTxnId; }

	 long getLastTxnId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLastTxnId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 long getId__wrappee__base(){ return id; }

	 long getId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
