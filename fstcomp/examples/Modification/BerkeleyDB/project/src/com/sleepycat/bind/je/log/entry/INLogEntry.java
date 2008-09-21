package com.sleepycat.je.log.entry; 
import java.nio.ByteBuffer; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.DatabaseId; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.log.LogUtils; 
import com.sleepycat.je.log.LoggableObject; 
import com.sleepycat.je.tree.IN; 
import com.sleepycat.je.utilint.DbLsn; 
import de.ovgu.cide.jakutil.*; 
public  class  INLogEntry  implements LogEntry, LoggableObject, NodeLogEntry, INContainingEntry {
	 private IN in;

	 private DatabaseId dbId;

	 private long obsoleteLsn;

	 private long nodeId;

	 private Class logClass;

	 public INLogEntry( Class logClass){ this.logClass=logClass; }

	 public INLogEntry( IN in){ this.in=in; this.dbId=in.getDatabase().getId(); this.logClass=in.getClass(); this.nodeId=in.getNodeId(); this.obsoleteLsn=in.getLastFullVersion(); }

	 public void readEntry__wrappee__base( ByteBuffer entryBuffer, int entrySize, byte entryTypeVersion, boolean readFullItem) throws DatabaseException { entryTypeVersion&=LogEntryType.clearProvisional(entryTypeVersion); try { if (readFullItem) { in=(IN)logClass.newInstance(); in.readFromLog(entryBuffer,entryTypeVersion); nodeId=in.getNodeId(); } else { int position=entryBuffer.position() + entrySize; if (entryTypeVersion == 1) { position-=LogUtils.UNSIGNED_INT_BYTES; } else if (entryTypeVersion >= 2) { position-=LogUtils.LONG_BYTES; } position-=LogUtils.INT_BYTES; nodeId=LogUtils.readLong(entryBuffer); entryBuffer.position(position); in=null; } dbId=new DatabaseId(); dbId.readFromLog(entryBuffer,entryTypeVersion); if (entryTypeVersion < 1) { obsoleteLsn=DbLsn.NULL_LSN; } else if (entryTypeVersion == 1) { long fileNum=LogUtils.getUnsignedInt(entryBuffer); if (fileNum == 0xffffffffL) { obsoleteLsn=DbLsn.NULL_LSN; } else { obsoleteLsn=DbLsn.makeLsn(fileNum,0); } } else { obsoleteLsn=LogUtils.readLong(entryBuffer); } } catch ( IllegalAccessException e) { throw new DatabaseException(e); }
catch ( InstantiationException e) { throw new DatabaseException(e); } }

	 public void readEntry( ByteBuffer entryBuffer, int entrySize, byte entryTypeVersion, boolean readFullItem) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getObsoleteLsn__wrappee__base(){ return obsoleteLsn; }

	 public long getObsoleteLsn(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getObsoleteLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public StringBuffer dumpEntry__wrappee__base( StringBuffer sb, boolean verbose){ in.dumpLog(sb,verbose); dbId.dumpLog(sb,verbose); return sb; }

	 public StringBuffer dumpEntry( StringBuffer sb, boolean verbose){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Object getMainItem__wrappee__base(){ return in; }

	 public Object getMainItem(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getMainItem__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Object clone__wrappee__base() throws CloneNotSupportedException { return super.clone(); }

	 public Object clone() throws CloneNotSupportedException { t.in(Thread.currentThread().getStackTrace()[1].toString());	clone__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isTransactional__wrappee__base(){ return false; }

	 public boolean isTransactional(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getTransactionId__wrappee__base(){ return 0; }

	 public long getTransactionId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTransactionId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LogEntryType getLogType__wrappee__base(){ return in.getLogType(); }

	 public LogEntryType getLogType(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean marshallOutsideWriteLatch__wrappee__base(){ return in.marshallOutsideWriteLatch(); }

	 public boolean marshallOutsideWriteLatch(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	marshallOutsideWriteLatch__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean countAsObsoleteWhenLogged__wrappee__base(){ return false; }

	 public boolean countAsObsoleteWhenLogged(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	countAsObsoleteWhenLogged__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void postLogWork__wrappee__base( long justLoggedLsn){ }

	 public void postLogWork( long justLoggedLsn){ t.in(Thread.currentThread().getStackTrace()[1].toString());	postLogWork__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getLogSize__wrappee__base(){ return (in.getLogSize() + dbId.getLogSize() + LogUtils.LONG_BYTES); }

	 public int getLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void writeToLog__wrappee__base( ByteBuffer destBuffer){ in.writeToLog(destBuffer); dbId.writeToLog(destBuffer); LogUtils.writeLong(destBuffer,obsoleteLsn); }

	 public void writeToLog( ByteBuffer destBuffer){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeToLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public IN getIN__wrappee__base( EnvironmentImpl env) throws DatabaseException { return in; }

	 public IN getIN( EnvironmentImpl env) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getNodeId__wrappee__base(){ return nodeId; }

	 public long getNodeId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNodeId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public DatabaseId getDbId__wrappee__base(){ return (DatabaseId)dbId; }

	 public DatabaseId getDbId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDbId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getLsnOfIN__wrappee__base( long lastReadLsn){ return lastReadLsn; }

	 public long getLsnOfIN( long lastReadLsn){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLsnOfIN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
