package com.sleepycat.je.txn; 
import java.nio.ByteBuffer; 
import java.sql.Timestamp; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.log.LogReadable; 
import com.sleepycat.je.log.LogUtils; 
import com.sleepycat.je.log.LoggableObject; 
import com.sleepycat.je.utilint.DbLsn; 
import de.ovgu.cide.jakutil.*; 
public abstract  class  TxnEnd  implements LoggableObject, LogReadable {
	 protected long id;

	 protected Timestamp time;

	 private long lastLsn;

	 TxnEnd( long id, long lastLsn){ this.id=id; time=new Timestamp(System.currentTimeMillis()); this.lastLsn=lastLsn; }

	 public TxnEnd(){ lastLsn=DbLsn.NULL_LSN; }

	 public long getId__wrappee__base(){ return id; }

	 public long getId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 long getLastLsn__wrappee__base(){ return lastLsn; }

	 long getLastLsn(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLastLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected abstract String getTagName__wrappee__base();

	 protected abstract String getTagName();{ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTagName__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract LogEntryType getLogType__wrappee__base();

	 public abstract LogEntryType getLogType();{ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean marshallOutsideWriteLatch__wrappee__base(){ return true; }

	 public boolean marshallOutsideWriteLatch(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	marshallOutsideWriteLatch__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean countAsObsoleteWhenLogged__wrappee__base(){ return false; }

	 public boolean countAsObsoleteWhenLogged(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	countAsObsoleteWhenLogged__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void postLogWork__wrappee__base( long justLoggedLsn){ }

	 public void postLogWork( long justLoggedLsn){ t.in(Thread.currentThread().getStackTrace()[1].toString());	postLogWork__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getLogSize__wrappee__base(){ return LogUtils.LONG_BYTES + LogUtils.getTimestampLogSize() + LogUtils.getLongLogSize(); }

	 public int getLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void writeToLog__wrappee__base( ByteBuffer logBuffer){ LogUtils.writeLong(logBuffer,id); LogUtils.writeTimestamp(logBuffer,time); LogUtils.writeLong(logBuffer,lastLsn); }

	 public void writeToLog( ByteBuffer logBuffer){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeToLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void readFromLog__wrappee__base( ByteBuffer logBuffer, byte entryTypeVersion){ id=LogUtils.readLong(logBuffer); time=LogUtils.readTimestamp(logBuffer); lastLsn=LogUtils.readLong(logBuffer); }

	 public void readFromLog( ByteBuffer logBuffer, byte entryTypeVersion){ t.in(Thread.currentThread().getStackTrace()[1].toString());	readFromLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void dumpLog__wrappee__base( StringBuffer sb, boolean verbose){ sb.append("<").append(getTagName()); sb.append(" id=\"").append(id); sb.append("\" time=\"").append(time); sb.append("\">"); sb.append(DbLsn.toString(lastLsn)); sb.append("</").append(getTagName()).append(">"); }

	 public void dumpLog( StringBuffer sb, boolean verbose){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean logEntryIsTransactional__wrappee__base(){ return true; }

	 public boolean logEntryIsTransactional(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	logEntryIsTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getTransactionId__wrappee__base(){ return id; }

	 public long getTransactionId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTransactionId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
