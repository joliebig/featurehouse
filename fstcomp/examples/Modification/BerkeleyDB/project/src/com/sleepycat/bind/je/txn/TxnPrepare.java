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

	 public Xid getXid__wrappee__base(){ return xid; }

	 public Xid getXid(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getXid__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LogEntryType getLogType__wrappee__base(){ return LogEntryType.LOG_TXN_PREPARE; }

	 public LogEntryType getLogType(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected String getTagName__wrappee__base(){ return "TxnPrepare"; }

	 protected String getTagName(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTagName__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getLogSize__wrappee__base(){ return LogUtils.LONG_BYTES + LogUtils.getTimestampLogSize() + LogUtils.getXidSize(xid); }

	 public int getLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void writeToLog__wrappee__base( ByteBuffer logBuffer){ LogUtils.writeLong(logBuffer,id); LogUtils.writeTimestamp(logBuffer,time); LogUtils.writeXid(logBuffer,xid); }

	 public void writeToLog( ByteBuffer logBuffer){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeToLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void readFromLog__wrappee__base( ByteBuffer logBuffer, byte entryTypeVersion){ id=LogUtils.readLong(logBuffer); time=LogUtils.readTimestamp(logBuffer); xid=LogUtils.readXid(logBuffer); }

	 public void readFromLog( ByteBuffer logBuffer, byte entryTypeVersion){ t.in(Thread.currentThread().getStackTrace()[1].toString());	readFromLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void dumpLog__wrappee__base( StringBuffer sb, boolean verbose){ sb.append("<").append(getTagName()); sb.append(" id=\"").append(id); sb.append("\" xid=\"").append(xid); sb.append("\" time=\"").append(time); sb.append("\">"); sb.append("</").append(getTagName()).append(">"); }

	 public void dumpLog( StringBuffer sb, boolean verbose){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
