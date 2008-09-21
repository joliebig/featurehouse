package com.sleepycat.je.tree; 
import java.nio.ByteBuffer; 
import com.sleepycat.je.dbi.DatabaseId; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.log.LogException; 
import com.sleepycat.je.log.LogReadable; 
import com.sleepycat.je.log.LogUtils; 
import com.sleepycat.je.log.LogWritable; 
import com.sleepycat.je.log.LoggableObject; 
import de.ovgu.cide.jakutil.*; 
public  class  INDeleteInfo  implements LoggableObject, LogReadable, LogWritable {
	 private long deletedNodeId;

	 private byte[] deletedIdKey;

	 private DatabaseId dbId;

	 public INDeleteInfo( long deletedNodeId, byte[] deletedIdKey, DatabaseId dbId){ this.deletedNodeId=deletedNodeId; this.deletedIdKey=deletedIdKey; this.dbId=dbId; }

	 public INDeleteInfo(){ dbId=new DatabaseId(); }

	 public long getDeletedNodeId__wrappee__base(){ return deletedNodeId; }

	 public long getDeletedNodeId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDeletedNodeId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getDeletedIdKey__wrappee__base(){ return deletedIdKey; }

	 public byte[] getDeletedIdKey(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDeletedIdKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public DatabaseId getDatabaseId__wrappee__base(){ return dbId; }

	 public DatabaseId getDatabaseId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDatabaseId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LogEntryType getLogType__wrappee__base(){ return LogEntryType.LOG_IN_DELETE_INFO; }

	 public LogEntryType getLogType(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean marshallOutsideWriteLatch__wrappee__base(){ return true; }

	 public boolean marshallOutsideWriteLatch(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	marshallOutsideWriteLatch__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean countAsObsoleteWhenLogged__wrappee__base(){ return false; }

	 public boolean countAsObsoleteWhenLogged(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	countAsObsoleteWhenLogged__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void postLogWork__wrappee__base( long justLoggedLsn){ }

	 public void postLogWork( long justLoggedLsn){ t.in(Thread.currentThread().getStackTrace()[1].toString());	postLogWork__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getLogSize__wrappee__base(){ return LogUtils.LONG_BYTES + LogUtils.getByteArrayLogSize(deletedIdKey) + dbId.getLogSize(); }

	 public int getLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void writeToLog__wrappee__base( ByteBuffer logBuffer){ LogUtils.writeLong(logBuffer,deletedNodeId); LogUtils.writeByteArray(logBuffer,deletedIdKey); dbId.writeToLog(logBuffer); }

	 public void writeToLog( ByteBuffer logBuffer){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeToLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void readFromLog__wrappee__base( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { deletedNodeId=LogUtils.readLong(itemBuffer); deletedIdKey=LogUtils.readByteArray(itemBuffer); dbId.readFromLog(itemBuffer,entryTypeVersion); }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readFromLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void dumpLog__wrappee__base( StringBuffer sb, boolean verbose){ sb.append("<INDeleteEntry node=\"").append(deletedNodeId); sb.append("\">"); sb.append(Key.dumpString(deletedIdKey,0)); dbId.dumpLog(sb,verbose); sb.append("</INDeleteEntry>"); }

	 public void dumpLog( StringBuffer sb, boolean verbose){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean logEntryIsTransactional__wrappee__base(){ return false; }

	 public boolean logEntryIsTransactional(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	logEntryIsTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getTransactionId__wrappee__base(){ return 0; }

	 public long getTransactionId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTransactionId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
