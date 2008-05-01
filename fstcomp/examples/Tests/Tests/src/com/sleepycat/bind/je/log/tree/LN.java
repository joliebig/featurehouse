package com.sleepycat.je.tree; 
import java.nio.ByteBuffer; 
import java.util.Map; 
import com.sleepycat.je.DatabaseEntry; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.cleaner.UtilizationTracker; 
import com.sleepycat.je.dbi.DatabaseId; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.dbi.INList; 
import com.sleepycat.je.dbi.MemoryBudget; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.log.LogException; 
import com.sleepycat.je.log.LogManager; 
import com.sleepycat.je.log.LogReadable; 
import com.sleepycat.je.log.LogUtils; 
import com.sleepycat.je.log.LoggableObject; 
import com.sleepycat.je.log.entry.DeletedDupLNLogEntry; 
import com.sleepycat.je.log.entry.LNLogEntry; 
import com.sleepycat.je.txn.Locker; 
import com.sleepycat.je.txn.Txn; 
import com.sleepycat.je.txn.WriteLockInfo; 
import com.sleepycat.je.utilint.DbLsn; 
import de.ovgu.cide.jakutil.*; 
public  class  LN  extends Node  implements LoggableObject, LogReadable {
	 private static final String BEGIN_TAG="<ln>";

	 private static final String END_TAG="</ln>";

	 private byte[] data;

	 public LN(){ super(false); this.data=null; }

	 public LN( byte[] data){ super(true); if (data == null) { this.data=null; } else { init(data,0,data.length); } }

	 public LN( DatabaseEntry dbt){ super(true); byte[] data=dbt.getData(); if (data == null) { this.data=null; } else if (dbt.getPartial()) { init(data,dbt.getOffset(),dbt.getPartialOffset() + dbt.getSize(),dbt.getPartialOffset(),dbt.getSize()); } else { init(data,dbt.getOffset(),dbt.getSize()); } }

	 private void init( byte[] data, int off, int len, int doff, int dlen){ if (len == 0) { this.data=LogUtils.ZERO_LENGTH_BYTE_ARRAY; } else { this.data=new byte[len]; System.arraycopy(data,off,this.data,doff,dlen); } }

	 private void init( byte[] data, int off, int len){ init(data,off,len,0,len); }

	 public byte[] getData(){ return data; }

	 public byte[] copyData(){ int len=data.length; byte[] ret=new byte[len]; System.arraycopy(data,0,ret,0,len); return ret; }

	 public boolean isDeleted(){ return (data == null); }

	 void makeDeleted(){ data=null; }

	 boolean isValidForDelete(){ return false; }

	 protected boolean isSoughtNode( long nid, boolean updateGeneration){ return false; }

	 protected boolean canBeAncestor( boolean targetContainsDuplicates){ return false; }

	 public long delete( DatabaseImpl database, byte[] lnKey, byte[] dupKey, long oldLsn, Locker locker) throws DatabaseException { makeDeleted(); EnvironmentImpl env=database.getDbEnvironment(); long newLsn=DbLsn.NULL_LSN; if (dupKey != null) { LogEntryType entryType; long logAbortLsn; boolean logAbortKnownDeleted; Txn logTxn; if (locker.isTransactional()) { entryType=LogEntryType.LOG_DEL_DUPLN_TRANSACTIONAL; WriteLockInfo info=locker.getWriteLockInfo(getNodeId()); logAbortLsn=info.getAbortLsn(); logAbortKnownDeleted=info.getAbortKnownDeleted(); logTxn=locker.getTxnLocker(); } else { entryType=LogEntryType.LOG_DEL_DUPLN; logAbortLsn=DbLsn.NULL_LSN; logAbortKnownDeleted=true; logTxn=null; } if (oldLsn == logAbortLsn) { oldLsn=DbLsn.NULL_LSN; } DeletedDupLNLogEntry logEntry=new DeletedDupLNLogEntry(entryType,this,database.getId(),dupKey,lnKey,logAbortLsn,logAbortKnownDeleted,logTxn); LogManager logManager=env.getLogManager(); newLsn=logManager.log(logEntry,false,oldLsn); } else { newLsn=log(env,database.getId(),lnKey,oldLsn,locker); } return newLsn; }

	 public long modify( byte[] newData, DatabaseImpl database, byte[] lnKey, long oldLsn, Locker locker) throws DatabaseException { data=newData; EnvironmentImpl env=database.getDbEnvironment(); long newLsn=log(env,database.getId(),lnKey,oldLsn,locker); return newLsn; }

	 void addToDirtyMap( Map dirtyMap){ }

	 void rebuildINList( INList inList){ }

	 void accountForSubtreeRemoval( INList inList, UtilizationTracker tracker){ }

	 public String beginTag(){ return BEGIN_TAG; }

	 public String endTag(){ return END_TAG; }

	 public String dumpString( int nSpaces, boolean dumpTags){ StringBuffer self=new StringBuffer(); if (dumpTags) { self.append(TreeUtils.indent(nSpaces)); self.append(beginTag()); self.append('\n'); } self.append(super.dumpString(nSpaces + 2,true)); self.append('\n'); if (data != null) { self.append(TreeUtils.indent(nSpaces + 2)); self.append("<data>"); self.append(TreeUtils.dumpByteArray(data)); self.append("</data>"); self.append('\n'); } if (dumpTags) { self.append(TreeUtils.indent(nSpaces)); self.append(endTag()); } return self.toString(); }

	 public long logProvisional( EnvironmentImpl env, DatabaseId dbId, byte[] key, long oldLsn) throws DatabaseException { return log(env,dbId,key,oldLsn,null,true); }

	 public long log( EnvironmentImpl env, DatabaseId dbId, byte[] key, long oldLsn, Locker locker) throws DatabaseException { return log(env,dbId,key,oldLsn,locker,false); }

	 private long log( EnvironmentImpl env, DatabaseId dbId, byte[] key, long oldLsn, Locker locker, boolean isProvisional) throws DatabaseException { LogEntryType entryType; long logAbortLsn; boolean logAbortKnownDeleted; Txn logTxn; if (locker != null && locker.isTransactional()) { entryType=getTransactionalLogType(); WriteLockInfo info=locker.getWriteLockInfo(getNodeId()); logAbortLsn=info.getAbortLsn(); logAbortKnownDeleted=info.getAbortKnownDeleted(); logTxn=locker.getTxnLocker(); assert logTxn != null; } else { entryType=getLogType(); logAbortLsn=DbLsn.NULL_LSN; logAbortKnownDeleted=false; logTxn=null; } if (oldLsn == logAbortLsn) { oldLsn=DbLsn.NULL_LSN; } LNLogEntry logEntry=new LNLogEntry(entryType,this,dbId,key,logAbortLsn,logAbortKnownDeleted,logTxn); LogManager logManager=env.getLogManager(); return logManager.log(logEntry,isProvisional,oldLsn); }

	 protected LogEntryType getTransactionalLogType(){ return LogEntryType.LOG_LN_TRANSACTIONAL; }

	 public boolean countAsObsoleteWhenLogged(){ return false; }

	 public LogEntryType getLogType(){ return LogEntryType.LOG_LN; }

	 public int getLogSize(){ int size=super.getLogSize(); size+=LogUtils.getBooleanLogSize(); if (!isDeleted()) { size+=LogUtils.getByteArrayLogSize(data); } return size; }

	 public void writeToLog( ByteBuffer logBuffer){ super.writeToLog(logBuffer); boolean dataExists=!isDeleted(); LogUtils.writeBoolean(logBuffer,dataExists); if (dataExists) { LogUtils.writeByteArray(logBuffer,data); } }

	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException { super.readFromLog(itemBuffer,entryTypeVersion); boolean dataExists=LogUtils.readBoolean(itemBuffer); if (dataExists) { data=LogUtils.readByteArray(itemBuffer); } }

	 public void dumpLog( StringBuffer sb, boolean verbose){ sb.append(beginTag()); super.dumpLog(sb,verbose); if (data != null) { sb.append("<data>"); sb.append(TreeUtils.dumpByteArray(data)); sb.append("</data>"); } dumpLogAdditional(sb,verbose); sb.append(endTag()); }

	 public boolean logEntryIsTransactional(){ return false; }

	 public long getTransactionId(){ return 0; }

	 protected void dumpLogAdditional( StringBuffer sb, boolean verbose){ }


}
