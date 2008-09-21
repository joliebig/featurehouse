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

	 public void addTargetType__wrappee__base( LogEntryType entryType) throws DatabaseException { targetEntryMap.put(entryType,entryType.getNewLogEntry()); }

	 public void addTargetType( LogEntryType entryType) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	addTargetType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected boolean isTargetEntry__wrappee__base( byte entryTypeNum, byte entryTypeVersion){ if (LogEntryType.isProvisional(entryTypeVersion)) { targetLogEntry=null; } else { LogEntryType fromLogType=new LogEntryType(entryTypeNum,entryTypeVersion); targetLogEntry=(LogEntry)targetEntryMap.get(fromLogType); } return (targetLogEntry != null); }

	 protected boolean isTargetEntry( byte entryTypeNum, byte entryTypeVersion){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isTargetEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected boolean processEntry__wrappee__base( ByteBuffer entryBuffer) throws DatabaseException { targetLogEntry.readEntry(entryBuffer,currentEntrySize,currentEntryTypeVersion,true); return true; }

	 protected boolean processEntry( ByteBuffer entryBuffer) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	processEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isLN__wrappee__base(){ return (targetLogEntry instanceof LNLogEntry); }

	 public boolean isLN(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isLN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LN getLN__wrappee__base(){ return ((LNLogEntry)targetLogEntry).getLN(); }

	 public LN getLN(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public DatabaseId getDatabaseId__wrappee__base(){ return ((LNLogEntry)targetLogEntry).getDbId(); }

	 public DatabaseId getDatabaseId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDatabaseId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getKey__wrappee__base(){ return ((LNLogEntry)targetLogEntry).getKey(); }

	 public byte[] getKey(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getDupTreeKey__wrappee__base(){ return ((LNLogEntry)targetLogEntry).getDupKey(); }

	 public byte[] getDupTreeKey(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDupTreeKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Long getTxnId__wrappee__base(){ return ((LNLogEntry)targetLogEntry).getTxnId(); }

	 public Long getTxnId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTxnId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isPrepare__wrappee__base(){ return (targetLogEntry.getMainItem() instanceof TxnPrepare); }

	 public boolean isPrepare(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isPrepare__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getTxnPrepareId__wrappee__base(){ return ((TxnPrepare)targetLogEntry.getMainItem()).getId(); }

	 public long getTxnPrepareId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTxnPrepareId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Xid getTxnPrepareXid__wrappee__base(){ return ((TxnPrepare)targetLogEntry.getMainItem()).getXid(); }

	 public Xid getTxnPrepareXid(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTxnPrepareXid__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isAbort__wrappee__base(){ return (targetLogEntry.getMainItem() instanceof TxnAbort); }

	 public boolean isAbort(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isAbort__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getTxnAbortId__wrappee__base(){ return ((TxnAbort)targetLogEntry.getMainItem()).getId(); }

	 public long getTxnAbortId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTxnAbortId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getTxnCommitId__wrappee__base(){ return ((TxnCommit)targetLogEntry.getMainItem()).getId(); }

	 public long getTxnCommitId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTxnCommitId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getNodeId__wrappee__base(){ return ((LNLogEntry)targetLogEntry).getLN().getNodeId(); }

	 public long getNodeId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNodeId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getAbortLsn__wrappee__base(){ return ((LNLogEntry)targetLogEntry).getAbortLsn(); }

	 public long getAbortLsn(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getAbortLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getAbortKnownDeleted__wrappee__base(){ return ((LNLogEntry)targetLogEntry).getAbortKnownDeleted(); }

	 public boolean getAbortKnownDeleted(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getAbortKnownDeleted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
