package com.sleepycat.je.recovery; 
import java.nio.ByteBuffer; 
import java.sql.Timestamp; 
import java.util.Calendar; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.log.LogException; 
import com.sleepycat.je.log.LogReadable; 
import com.sleepycat.je.log.LogUtils; 
import com.sleepycat.je.log.LoggableObject; 
import de.ovgu.cide.jakutil.*; 
public  class  CheckpointStart  implements LoggableObject, LogReadable {
	 private Timestamp startTime;

	 private long id;

	 private String invoker;

	 public CheckpointStart( long id, String invoker){ Calendar cal=Calendar.getInstance(); this.startTime=new Timestamp(cal.getTime().getTime()); this.id=id; if (invoker == null) { this.invoker=""; } else { this.invoker=invoker; } }

	 public CheckpointStart(){ }

	 public LogEntryType getLogType(){ return LogEntryType.LOG_CKPT_START; }

	 public boolean marshallOutsideWriteLatch(){ return true; }

	 public boolean countAsObsoleteWhenLogged(){ return false; }

	 public void postLogWork( long justLoggedLsn){ }

	 public int getLogSize(){ return LogUtils.getTimestampLogSize() + LogUtils.getLongLogSize() + LogUtils.getStringLogSize(invoker); }

	 public void writeToLog( ByteBuffer logBuffer){ LogUtils.writeTimestamp(logBuffer,startTime); LogUtils.writeLong(logBuffer,id); LogUtils.writeString(logBuffer,invoker); }

	 public void readFromLog( ByteBuffer logBuffer, byte entryTypeVersion) throws LogException { startTime=LogUtils.readTimestamp(logBuffer); id=LogUtils.readLong(logBuffer); invoker=LogUtils.readString(logBuffer); }

	 public void dumpLog( StringBuffer sb, boolean verbose){ sb.append("<CkptStart invoker=\"").append(invoker); sb.append("\" time=\"").append(startTime); sb.append("\" id=\"").append(id); sb.append("\"/>"); }

	 public boolean logEntryIsTransactional(){ return false; }

	 public long getTransactionId(){ return 0; }


}
