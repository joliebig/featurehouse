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

	 synchronized public void setLastTxnId( long lastId){ this.lastUsedTxnId=lastId; }

	 public synchronized long getLastTxnId(){ return lastUsedTxnId; }

	 synchronized long incTxnId(){ return ++lastUsedTxnId; }

	 public Txn txnBegin( Transaction parent, TransactionConfig txnConfig) throws DatabaseException { if (parent != null) { throw new DatabaseException("Nested transactions are not supported yet."); } return new Txn(env,txnConfig); }

	 public LockManager getLockManager(){ return lockManager; }

	 void registerTxn( Txn txn) throws DatabaseException { allTxns.add(txn); if (txn.isSerializableIsolation()) { nActiveSerializable++; } }

	 void unRegisterTxn( Txn txn, boolean isCommit) throws DatabaseException { allTxns.remove(txn); this.hook828(txn); this.hook825(isCommit); if (txn.isSerializableIsolation()) { nActiveSerializable--; } }

	 public void registerXATxn( Xid xid, Txn txn, boolean isPrepare) throws DatabaseException { if (!allXATxns.containsKey(xid)) { allXATxns.put(xid,txn); this.hook829(); } this.hook826(isPrepare); }

	 void unRegisterXATxn( Xid xid, boolean isCommit) throws DatabaseException { if (allXATxns.remove(xid) == null) { throw new DatabaseException("XA Transaction " + xid + " can not be unregistered."); } this.hook830(); this.hook827(isCommit); }

	 public Txn getTxnFromXid( Xid xid) throws DatabaseException { return (Txn)allXATxns.get(xid); }

	 public void setTxnForThread( Transaction txn){ Thread curThread=Thread.currentThread(); thread2Txn.put(curThread,txn); }

	 public Transaction unsetTxnForThread() throws DatabaseException { Thread curThread=Thread.currentThread(); return (Transaction)thread2Txn.remove(curThread); }

	 public Transaction getTxnForThread() throws DatabaseException { return (Transaction)thread2Txn.get(Thread.currentThread()); }

	 public Xid[] XARecover() throws DatabaseException { Set xidSet=allXATxns.keySet(); Xid[] ret=new Xid[xidSet.size()]; ret=(Xid[])xidSet.toArray(ret); return ret; }

	 public boolean areOtherSerializableTransactionsActive( Locker excludeLocker){ int exclude=(excludeLocker != null && excludeLocker.isSerializableIsolation()) ? 1 : 0; return (nActiveSerializable - exclude > 0); }

	 public long getFirstActiveLsn() throws DatabaseException { long firstActive=DbLsn.NULL_LSN; firstActive=this.hook823(firstActive); return firstActive; }

	 protected void hook821( EnvironmentImpl env) throws DatabaseException { }

	 protected void hook822( EnvironmentImpl env) throws DatabaseException { if (env.isNoLocking()) { lockManager=new DummyLockManager(env); } else { lockManager=new SyncedLockManager(env); } }

	 protected long hook823( long firstActive) throws DatabaseException { Iterator iter=allTxns.iterator(); while (iter.hasNext()) { long txnFirstActive=((Txn)iter.next()).getFirstActiveLsn(); if (firstActive == DbLsn.NULL_LSN) { firstActive=txnFirstActive; } else if (txnFirstActive != DbLsn.NULL_LSN) { if (DbLsn.compareTo(txnFirstActive,firstActive) < 0) { firstActive=txnFirstActive; } } } return firstActive; }

	 protected void hook824() throws DatabaseException { }

	 protected void hook825( boolean isCommit) throws DatabaseException { }

	 protected void hook826( boolean isPrepare) throws DatabaseException { }

	 protected void hook827( boolean isCommit) throws DatabaseException { }

	 protected void hook828( Txn txn) throws DatabaseException { }

	 protected void hook829() throws DatabaseException { }

	 protected void hook830() throws DatabaseException { }


}
