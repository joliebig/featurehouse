package com.sleepycat.je.log; 
import java.io.IOException; 
import java.nio.ByteBuffer; 
import java.util.HashMap; 
import java.util.Map; 
import javax.transaction.xa.Xid; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.DatabaseId; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.log.entry.LNLogEntry; 
import com.sleepycat.je.log.entry.LogEntry; 
import com.sleepycat.je.tree.LN; 
import com.sleepycat.je.txn.TxnAbort; 
import com.sleepycat.je.txn.TxnCommit; 
import com.sleepycat.je.txn.TxnPrepare; 
import de.ovgu.cide.jakutil.*; 
public  class  LNFileReader  extends FileReader {
	 protected Map targetEntryMap;

	 protected LogEntry targetLogEntry;

	 public LNFileReader( EnvironmentImpl env, int readBufferSize, long startLsn, boolean redo, long endOfFileLsn, long finishLsn, Long singleFileNum) throws IOException, DatabaseException { super(env,readBufferSize,redo,startLsn,singleFileNum,endOfFileLsn,finishLsn); targetEntryMap=new HashMap(); }

	 public void addTargetType( LogEntryType entryType) throws DatabaseException { targetEntryMap.put(entryType,entryType.getNewLogEntry()); }

	 protected boolean isTargetEntry( byte entryTypeNum, byte entryTypeVersion){ if (LogEntryType.isProvisional(entryTypeVersion)) { targetLogEntry=null; } else { LogEntryType fromLogType=new LogEntryType(entryTypeNum,entryTypeVersion); targetLogEntry=(LogEntry)targetEntryMap.get(fromLogType); } return (targetLogEntry != null); }

	 protected boolean processEntry( ByteBuffer entryBuffer) throws DatabaseException { targetLogEntry.readEntry(entryBuffer,currentEntrySize,currentEntryTypeVersion,true); return true; }

	 public boolean isLN(){ return (targetLogEntry instanceof LNLogEntry); }

	 public LN getLN(){ return ((LNLogEntry)targetLogEntry).getLN(); }

	 public DatabaseId getDatabaseId(){ return ((LNLogEntry)targetLogEntry).getDbId(); }

	 public byte[] getKey(){ return ((LNLogEntry)targetLogEntry).getKey(); }

	 public byte[] getDupTreeKey(){ return ((LNLogEntry)targetLogEntry).getDupKey(); }

	 public Long getTxnId(){ return ((LNLogEntry)targetLogEntry).getTxnId(); }

	 public boolean isPrepare(){ return (targetLogEntry.getMainItem() instanceof TxnPrepare); }

	 public long getTxnPrepareId(){ return ((TxnPrepare)targetLogEntry.getMainItem()).getId(); }

	 public Xid getTxnPrepareXid(){ return ((TxnPrepare)targetLogEntry.getMainItem()).getXid(); }

	 public boolean isAbort(){ return (targetLogEntry.getMainItem() instanceof TxnAbort); }

	 public long getTxnAbortId(){ return ((TxnAbort)targetLogEntry.getMainItem()).getId(); }

	 public long getTxnCommitId(){ return ((TxnCommit)targetLogEntry.getMainItem()).getId(); }

	 public long getNodeId(){ return ((LNLogEntry)targetLogEntry).getLN().getNodeId(); }

	 public long getAbortLsn(){ return ((LNLogEntry)targetLogEntry).getAbortLsn(); }

	 public boolean getAbortKnownDeleted(){ return ((LNLogEntry)targetLogEntry).getAbortKnownDeleted(); }


}
