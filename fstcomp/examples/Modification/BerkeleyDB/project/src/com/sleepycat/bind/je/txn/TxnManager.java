package com.sleepycat.je.txn; 
import java.util.Collections; 
import java.util.HashMap; 
import java.util.HashSet; 
import java.util.Iterator; 
import java.util.Map; 
import java.util.Set; 
import javax.transaction.xa.Xid; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.LockStats; 
import com.sleepycat.je.Transaction; 
import com.sleepycat.je.TransactionConfig; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.dbi.MemoryBudget; 
import com.sleepycat.je.utilint.DbLsn; 
import de.ovgu.cide.jakutil.*; 
public  class  TxnManager {
	 static final long NULL_TXN_ID=-1;

	 private static final String DEBUG_NAME=TxnManager.class.getName();

	 private LockManager lockManager;

	 private EnvironmentImpl env;

	 private Set allTxns;

	 private Map allXATxns;

	 private Map thread2Txn;

	 private long lastUsedTxnId;

	 private int nActiveSerializable;

	 public TxnManager( EnvironmentImpl env) throws DatabaseException { this.hook822(env); this.env=env; allTxns=new HashSet(); this.hook821(env); allXATxns=Collections.synchronizedMap(new HashMap()); thread2Txn=Collections.synchronizedMap(new HashMap()); this.hook824(); lastUsedTxnId=0; }

	 synchronized public void setLastTxnId__wrappee__base( long lastId){ this.lastUsedTxnId=lastId; }

	 synchronized public void setLastTxnId( long lastId){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setLastTxnId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public synchronized long getLastTxnId__wrappee__base(){ return lastUsedTxnId; }

	 public synchronized long getLastTxnId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLastTxnId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 synchronized long incTxnId__wrappee__base(){ return ++lastUsedTxnId; }

	 synchronized long incTxnId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	incTxnId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Txn txnBegin__wrappee__base( Transaction parent, TransactionConfig txnConfig) throws DatabaseException { if (parent != null) { throw new DatabaseException("Nested transactions are not supported yet."); } return new Txn(env,txnConfig); }

	 public Txn txnBegin( Transaction parent, TransactionConfig txnConfig) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	txnBegin__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LockManager getLockManager__wrappee__base(){ return lockManager; }

	 public LockManager getLockManager(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLockManager__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void registerTxn__wrappee__base( Txn txn) throws DatabaseException { allTxns.add(txn); if (txn.isSerializableIsolation()) { nActiveSerializable++; } }

	 void registerTxn( Txn txn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	registerTxn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void unRegisterTxn__wrappee__base( Txn txn, boolean isCommit) throws DatabaseException { allTxns.remove(txn); this.hook828(txn); this.hook825(isCommit); if (txn.isSerializableIsolation()) { nActiveSerializable--; } }

	 void unRegisterTxn( Txn txn, boolean isCommit) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	unRegisterTxn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void registerXATxn__wrappee__base( Xid xid, Txn txn, boolean isPrepare) throws DatabaseException { if (!allXATxns.containsKey(xid)) { allXATxns.put(xid,txn); this.hook829(); } this.hook826(isPrepare); }

	 public void registerXATxn( Xid xid, Txn txn, boolean isPrepare) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	registerXATxn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void unRegisterXATxn__wrappee__base( Xid xid, boolean isCommit) throws DatabaseException { if (allXATxns.remove(xid) == null) { throw new DatabaseException("XA Transaction " + xid + " can not be unregistered."); } this.hook830(); this.hook827(isCommit); }

	 void unRegisterXATxn( Xid xid, boolean isCommit) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	unRegisterXATxn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Txn getTxnFromXid__wrappee__base( Xid xid) throws DatabaseException { return (Txn)allXATxns.get(xid); }

	 public Txn getTxnFromXid( Xid xid) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getTxnFromXid__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void setTxnForThread__wrappee__base( Transaction txn){ Thread curThread=Thread.currentThread(); thread2Txn.put(curThread,txn); }

	 public void setTxnForThread( Transaction txn){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setTxnForThread__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Transaction unsetTxnForThread__wrappee__base() throws DatabaseException { Thread curThread=Thread.currentThread(); return (Transaction)thread2Txn.remove(curThread); }

	 public Transaction unsetTxnForThread() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	unsetTxnForThread__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Transaction getTxnForThread__wrappee__base() throws DatabaseException { return (Transaction)thread2Txn.get(Thread.currentThread()); }

	 public Transaction getTxnForThread() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getTxnForThread__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Xid\[\] XARecover__wrappee__base() throws DatabaseException { Set xidSet=allXATxns.keySet(); Xid[] ret=new Xid[xidSet.size()]; ret=(Xid[])xidSet.toArray(ret); return ret; }

	 public Xid[] XARecover() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	XARecover__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean areOtherSerializableTransactionsActive__wrappee__base( Locker excludeLocker){ int exclude=(excludeLocker != null && excludeLocker.isSerializableIsolation()) ? 1 : 0; return (nActiveSerializable - exclude > 0); }

	 public boolean areOtherSerializableTransactionsActive( Locker excludeLocker){ t.in(Thread.currentThread().getStackTrace()[1].toString());	areOtherSerializableTransactionsActive__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getFirstActiveLsn__wrappee__base() throws DatabaseException { long firstActive=DbLsn.NULL_LSN; firstActive=this.hook823(firstActive); return firstActive; }

	 public long getFirstActiveLsn() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getFirstActiveLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook821__wrappee__base( EnvironmentImpl env) throws DatabaseException { }

	 protected void hook821( EnvironmentImpl env) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook821__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook822__wrappee__base( EnvironmentImpl env) throws DatabaseException { if (env.isNoLocking()) { lockManager=new DummyLockManager(env); } else { lockManager=new SyncedLockManager(env); } }

	 protected void hook822( EnvironmentImpl env) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook822__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected long hook823__wrappee__base( long firstActive) throws DatabaseException { Iterator iter=allTxns.iterator(); while (iter.hasNext()) { long txnFirstActive=((Txn)iter.next()).getFirstActiveLsn(); if (firstActive == DbLsn.NULL_LSN) { firstActive=txnFirstActive; } else if (txnFirstActive != DbLsn.NULL_LSN) { if (DbLsn.compareTo(txnFirstActive,firstActive) < 0) { firstActive=txnFirstActive; } } } return firstActive; }

	 protected long hook823( long firstActive) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook823__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook824__wrappee__base() throws DatabaseException { }

	 protected void hook824() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook824__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook825__wrappee__base( boolean isCommit) throws DatabaseException { }

	 protected void hook825( boolean isCommit) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook825__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook826__wrappee__base( boolean isPrepare) throws DatabaseException { }

	 protected void hook826( boolean isPrepare) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook826__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook827__wrappee__base( boolean isCommit) throws DatabaseException { }

	 protected void hook827( boolean isCommit) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook827__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook828__wrappee__base( Txn txn) throws DatabaseException { }

	 protected void hook828( Txn txn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook828__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook829__wrappee__base() throws DatabaseException { }

	 protected void hook829() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook829__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook830__wrappee__base() throws DatabaseException { }

	 protected void hook830() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook830__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
