package com.sleepycat.je.log.entry; 
import java.nio.ByteBuffer; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.DatabaseId; 
import com.sleepycat.je.log.LogEntryType; 
import com.sleepycat.je.log.LogUtils; 
import com.sleepycat.je.log.LoggableObject; 
import com.sleepycat.je.tree.Key; 
import com.sleepycat.je.tree.LN; 
import com.sleepycat.je.txn.Txn; 
import com.sleepycat.je.utilint.DbLsn; 
import de.ovgu.cide.jakutil.*; 
public  class  LNLogEntry  implements LogEntry, LoggableObject, NodeLogEntry {
	 private LN ln;

	 private DatabaseId dbId;

	 private byte[] key;

	 private long abortLsn=DbLsn.NULL_LSN;

	 private boolean abortKnownDeleted;

	 private Txn txn;

	 private static final byte ABORT_KNOWN_DELETED_MASK=(byte)1;

	 private Class logClass;

	 private LogEntryType entryType;

	 private long nodeId;

	 private boolean isTransactional;

	 public LNLogEntry( Class logClass, boolean isTransactional){ this.logClass=logClass; this.isTransactional=isTransactional; }

	 public LNLogEntry( LogEntryType entryType, LN ln, DatabaseId dbId, byte[] key, long abortLsn, boolean abortKnownDeleted, Txn txn){ this.entryType=entryType; this.ln=ln; this.dbId=dbId; this.key=key; this.abortLsn=abortLsn; this.abortKnownDeleted=abortKnownDeleted; this.txn=txn; this.isTransactional=(txn != null); this.logClass=ln.getClass(); this.nodeId=ln.getNodeId(); }

	 public void readEntry__wrappee__base( ByteBuffer entryBuffer, int entrySize, byte entryTypeVersion, boolean readFullItem) throws DatabaseException { try { if (readFullItem) { ln=(LN)logClass.newInstance(); ln.readFromLog(entryBuffer,entryTypeVersion); nodeId=ln.getNodeId(); dbId=new DatabaseId(); dbId.readFromLog(entryBuffer,entryTypeVersion); key=LogUtils.readByteArray(entryBuffer); if (isTransactional) { abortLsn=LogUtils.readLong(entryBuffer); if (DbLsn.getFileNumber(abortLsn) == DbLsn.getFileNumber(DbLsn.NULL_LSN)) { abortLsn=DbLsn.NULL_LSN; } abortKnownDeleted=((entryBuffer.get() & ABORT_KNOWN_DELETED_MASK) != 0) ? true : false; txn=new Txn(); txn.readFromLog(entryBuffer,entryTypeVersion); } } else { int endPosition=entryBuffer.position() + entrySize; nodeId=LogUtils.readLong(entryBuffer); entryBuffer.position(endPosition); ln=null; } } catch ( IllegalAccessException e) { throw new DatabaseException(e); }
catch ( InstantiationException e) { throw new DatabaseException(e); } }

	 public void readEntry( ByteBuffer entryBuffer, int entrySize, byte entryTypeVersion, boolean readFullItem) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public StringBuffer dumpEntry__wrappee__base( StringBuffer sb, boolean verbose){ ln.dumpLog(sb,verbose); dbId.dumpLog(sb,verbose); sb.append(Key.dumpString(key,0)); if (isTransactional) { if (abortLsn != DbLsn.NULL_LSN) { sb.append(DbLsn.toString(abortLsn)); } sb.append("<knownDeleted val=\""); sb.append(abortKnownDeleted ? "true" : "false"); sb.append("\"/>"); txn.dumpLog(sb,verbose); } return sb; }

	 public StringBuffer dumpEntry( StringBuffer sb, boolean verbose){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Object getMainItem__wrappee__base(){ return ln; }

	 public Object getMainItem(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getMainItem__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Object clone__wrappee__base() throws CloneNotSupportedException { return super.clone(); }

	 public Object clone() throws CloneNotSupportedException { t.in(Thread.currentThread().getStackTrace()[1].toString());	clone__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isTransactional__wrappee__base(){ return isTransactional; }

	 public boolean isTransactional(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getTransactionId__wrappee__base(){ if (isTransactional) { return txn.getId(); } else { return 0; } }

	 public long getTransactionId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTransactionId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getNodeId__wrappee__base(){ return nodeId; }

	 public long getNodeId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNodeId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LogEntryType getLogType__wrappee__base(){ return entryType; }

	 public LogEntryType getLogType(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean marshallOutsideWriteLatch__wrappee__base(){ return ln.marshallOutsideWriteLatch(); }

	 public boolean marshallOutsideWriteLatch(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	marshallOutsideWriteLatch__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean countAsObsoleteWhenLogged__wrappee__base(){ return ln.isDeleted(); }

	 public boolean countAsObsoleteWhenLogged(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	countAsObsoleteWhenLogged__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void postLogWork__wrappee__base( long justLoggedLsn) throws DatabaseException { if (isTransactional) { txn.addLogInfo(justLoggedLsn); } }

	 public void postLogWork( long justLoggedLsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	postLogWork__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getLogSize__wrappee__base(){ int size=ln.getLogSize() + dbId.getLogSize() + LogUtils.getByteArrayLogSize(key); if (isTransactional) { size+=LogUtils.getLongLogSize(); size++; size+=txn.getLogSize(); } return size; }

	 public int getLogSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void writeToLog__wrappee__base( ByteBuffer destBuffer){ ln.writeToLog(destBuffer); dbId.writeToLog(destBuffer); LogUtils.writeByteArray(destBuffer,key); if (isTransactional) { LogUtils.writeLong(destBuffer,abortLsn); byte aKD=0; if (abortKnownDeleted) { aKD|=ABORT_KNOWN_DELETED_MASK; } destBuffer.put(aKD); txn.writeToLog(destBuffer); } }

	 public void writeToLog( ByteBuffer destBuffer){ t.in(Thread.currentThread().getStackTrace()[1].toString());	writeToLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LN getLN__wrappee__base(){ return ln; }

	 public LN getLN(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLN__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public DatabaseId getDbId__wrappee__base(){ return dbId; }

	 public DatabaseId getDbId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDbId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getKey__wrappee__base(){ return key; }

	 public byte[] getKey(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public byte\[\] getDupKey__wrappee__base(){ if (ln.isDeleted()) { return null; } else { return ln.getData(); } }

	 public byte[] getDupKey(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDupKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getAbortLsn__wrappee__base(){ return abortLsn; }

	 public long getAbortLsn(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getAbortLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getAbortKnownDeleted__wrappee__base(){ return abortKnownDeleted; }

	 public boolean getAbortKnownDeleted(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getAbortKnownDeleted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Long getTxnId__wrappee__base(){ if (isTransactional) { return new Long(txn.getId()); } else { return null; } }

	 public Long getTxnId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTxnId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Txn getUserTxn__wrappee__base(){ if (isTransactional) { return txn; } else { return null; } }

	 public Txn getUserTxn(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getUserTxn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
