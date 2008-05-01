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

	 public long getId(){ return id; }

	 long getLastLsn(){ return lastLsn; }

	 protected abstract String getTagName();

	 public abstract LogEntryType getLogType();

	 public boolean marshallOutsideWriteLatch(){ return true; }

	 public boolean countAsObsoleteWhenLogged(){ return false; }

	 public void postLogWork( long justLoggedLsn){ }

	 public int getLogSize(){ return LogUtils.LONG_BYTES + LogUtils.getTimestampLogSize() + LogUtils.getLongLogSize(); }

	 public void writeToLog( ByteBuffer logBuffer){ LogUtils.writeLong(logBuffer,id); LogUtils.writeTimestamp(logBuffer,time); LogUtils.writeLong(logBuffer,lastLsn); }

	 public void readFromLog( ByteBuffer logBuffer, byte entryTypeVersion){ id=LogUtils.readLong(logBuffer); time=LogUtils.readTimestamp(logBuffer); lastLsn=LogUtils.readLong(logBuffer); }

	 public void dumpLog( StringBuffer sb, boolean verbose){ sb.append("<").append(getTagName()); sb.append(" id=\"").append(id); sb.append("\" time=\"").append(time); sb.append("\">"); sb.append(DbLsn.toString(lastLsn)); sb.append("</").append(getTagName()).append(">"); }

	 public boolean logEntryIsTransactional(){ return true; }

	 public long getTransactionId(){ return id; }


}
