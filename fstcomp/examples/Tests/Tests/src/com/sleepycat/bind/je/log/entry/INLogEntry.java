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

	 public void readEntry( ByteBuffer entryBuffer, int entrySize, byte entryTypeVersion, boolean readFullItem) throws DatabaseException { entryTypeVersion&=LogEntryType.clearProvisional(entryTypeVersion); try { if (readFullItem) { in=(IN)logClass.newInstance(); in.readFromLog(entryBuffer,entryTypeVersion); nodeId=in.getNodeId(); } else { int position=entryBuffer.position() + entrySize; if (entryTypeVersion == 1) { position-=LogUtils.UNSIGNED_INT_BYTES; } else if (entryTypeVersion >= 2) { position-=LogUtils.LONG_BYTES; } position-=LogUtils.INT_BYTES; nodeId=LogUtils.readLong(entryBuffer); entryBuffer.position(position); in=null; } dbId=new DatabaseId(); dbId.readFromLog(entryBuffer,entryTypeVersion); if (entryTypeVersion < 1) { obsoleteLsn=DbLsn.NULL_LSN; } else if (entryTypeVersion == 1) { long fileNum=LogUtils.getUnsignedInt(entryBuffer); if (fileNum == 0xffffffffL) { obsoleteLsn=DbLsn.NULL_LSN; } else { obsoleteLsn=DbLsn.makeLsn(fileNum,0); } } else { obsoleteLsn=LogUtils.readLong(entryBuffer); } } catch ( IllegalAccessException e) { throw new DatabaseException(e); }
catch ( InstantiationException e) { throw new DatabaseException(e); } }

	 public long getObsoleteLsn(){ return obsoleteLsn; }

	 public StringBuffer dumpEntry( StringBuffer sb, boolean verbose){ in.dumpLog(sb,verbose); dbId.dumpLog(sb,verbose); return sb; }

	 public Object getMainItem(){ return in; }

	 public Object clone() throws CloneNotSupportedException { return super.clone(); }

	 public boolean isTransactional(){ return false; }

	 public long getTransactionId(){ return 0; }

	 public LogEntryType getLogType(){ return in.getLogType(); }

	 public boolean marshallOutsideWriteLatch(){ return in.marshallOutsideWriteLatch(); }

	 public boolean countAsObsoleteWhenLogged(){ return false; }

	 public void postLogWork( long justLoggedLsn){ }

	 public int getLogSize(){ return (in.getLogSize() + dbId.getLogSize() + LogUtils.LONG_BYTES); }

	 public void writeToLog( ByteBuffer destBuffer){ in.writeToLog(destBuffer); dbId.writeToLog(destBuffer); LogUtils.writeLong(destBuffer,obsoleteLsn); }

	 public IN getIN( EnvironmentImpl env) throws DatabaseException { return in; }

	 public long getNodeId(){ return nodeId; }

	 public DatabaseId getDbId(){ return (DatabaseId)dbId; }

	 public long getLsnOfIN( long lastReadLsn){ return lastReadLsn; }


}
