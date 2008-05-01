package com.sleepycat.je.txn; 
import java.nio.ByteBuffer; 
import javax.transaction.xa.Xid; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.log.LogReadable; 
import com.sleepycat.je.log.LogUtils; 
import com.sleepycat.je.log.LoggableObject; 
import com.sleepycat.je.utilint.DbLsn; 
import de.ovgu.cide.jakutil.*; 
public  class  TxnPrepare  extends TxnEnd {
	 private Xid xid;

	 public TxnPrepare( long id, Xid xid){ super(id,DbLsn.NULL_LSN); this.xid=xid; }

	 public TxnPrepare(){ }

	 public Xid getXid(){ return xid; }

	 public LogEntryType getLogType(){ return LogEntryType.LOG_TXN_PREPARE; }

	 protected String getTagName(){ return "TxnPrepare"; }

	 public int getLogSize(){ return LogUtils.LONG_BYTES + LogUtils.getTimestampLogSize() + LogUtils.getXidSize(xid); }

	 public void writeToLog( ByteBuffer logBuffer){ LogUtils.writeLong(logBuffer,id); LogUtils.writeTimestamp(logBuffer,time); LogUtils.writeXid(logBuffer,xid); }

	 public void readFromLog( ByteBuffer logBuffer, byte entryTypeVersion){ id=LogUtils.readLong(logBuffer); time=LogUtils.readTimestamp(logBuffer); xid=LogUtils.readXid(logBuffer); }

	 public void dumpLog( StringBuffer sb, boolean verbose){ sb.append("<").append(getTagName()); sb.append(" id=\"").append(id); sb.append("\" xid=\"").append(xid); sb.append("\" time=\"").append(time); sb.append("\">"); sb.append("</").append(getTagName()).append(">"); }


}
