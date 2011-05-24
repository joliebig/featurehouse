package com.sleepycat.je.txn; 
import java.util.HashMap; 
import java.util.HashSet; 
import java.util.Hashtable; 
import java.util.Iterator; 
import java.util.Map; 
import java.util.Set; 
import com.sleepycat.je.Database; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.DeadlockException; 
import com.sleepycat.je.DbInternal; 
import com.sleepycat.je.LockNotGrantedException; 
import com.sleepycat.je.LockStats; 
import com.sleepycat.je.OperationStatus; 
import com.sleepycat.je.dbi.CursorImpl; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.tree.BIN; 
import com.sleepycat.je.tree.BINReference; 
import com.sleepycat.je.tree.Key; 
import de.ovgu.cide.jakutil.*; 
public abstract  class  Locker {
	 private static final String DEBUG_NAME=Locker.class.getName();

	 protected EnvironmentImpl envImpl;

	 protected LockManager lockManager;

	 protected long id;

	 protected boolean readUncommittedDefault;

	 protected boolean defaultNoWait;

	 protected long lockTimeOutMillis;

	 private long txnTimeOutMillis;

	 private long txnStartMillis;

	 private Lock waitingFor;

	 protected Map handleLockToHandleMap;

	 protected Map handleToHandleLockMap;

	 protected Thread thread;

	 public Locker( EnvironmentImpl envImpl, boolean readUncommittedDefault, boolean noWait) throws DatabaseException { TxnManager txnManager=envImpl.getTxnManager(); this.id=generateId(txnManager); this.envImpl=envImpl; lockManager=txnManager.getLockManager(); this.readUncommittedDefault=readUncommittedDefault; this.waitingFor=null; defaultNoWait=noWait; lockTimeOutMillis=envImpl.getLockTimeout(); txnTimeOutMillis=envImpl.getTxnTimeout(); if (txnTimeOutMillis != 0) { txnStartMillis=System.currentTimeMillis(); } else { txnStartMillis=0; } thread=Thread.currentThread(); }

	 Locker(){ }

	 protected abstract long generateId__wrappee__base( TxnManager txnManager);

	 protected abstract long generateId( TxnManager txnManager);{ t.in(Thread.currentThread().getStackTrace()[1].toString());	generateId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getId__wrappee__base(){ return id; }

	 public long getId(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getId__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean getDefaultNoWait__wrappee__base(){ return defaultNoWait; }

	 public boolean getDefaultNoWait(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDefaultNoWait__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public synchronized long getLockTimeout__wrappee__base(){ return lockTimeOutMillis; }

	 public synchronized long getLockTimeout(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLockTimeout__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public synchronized void setLockTimeout__wrappee__base( long timeOutMillis){ lockTimeOutMillis=timeOutMillis; }

	 public synchronized void setLockTimeout( long timeOutMillis){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setLockTimeout__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public synchronized void setTxnTimeout__wrappee__base( long timeOutMillis){ txnTimeOutMillis=timeOutMillis; txnStartMillis=System.currentTimeMillis(); }

	 public synchronized void setTxnTimeout( long timeOutMillis){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setTxnTimeout__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isReadUncommittedDefault__wrappee__base(){ return readUncommittedDefault; }

	 public boolean isReadUncommittedDefault(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isReadUncommittedDefault__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 Lock getWaitingFor__wrappee__base(){ return waitingFor; }

	 Lock getWaitingFor(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getWaitingFor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setWaitingFor__wrappee__base( Lock lock){ waitingFor=lock; }

	 void setWaitingFor( Lock lock){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setWaitingFor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setOnlyAbortable__wrappee__base(){ }

	 void setOnlyAbortable(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setOnlyAbortable__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected abstract void checkState__wrappee__base( boolean ignoreCalledByAbort) throws DatabaseException ;

	 protected abstract void checkState( boolean ignoreCalledByAbort) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	checkState__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 abstract LockResult lockInternal__wrappee__base( long nodeId, LockType lockType, boolean noWait, DatabaseImpl database) throws DeadlockException, DatabaseException ;

	 abstract LockResult lockInternal( long nodeId, LockType lockType, boolean noWait, DatabaseImpl database) throws DeadlockException, DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	lockInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LockResult lock__wrappee__base( long nodeId, LockType lockType, boolean noWait, DatabaseImpl database) throws LockNotGrantedException, DeadlockException, DatabaseException { LockResult result=lockInternal(nodeId,lockType,noWait,database); if (result.getLockGrant() == LockGrantType.DENIED) { throw new LockNotGrantedException("Non-blocking lock was denied."); } else { return result; } }

	 public LockResult lock( long nodeId, LockType lockType, boolean noWait, DatabaseImpl database) throws LockNotGrantedException, DeadlockException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	lock__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LockResult nonBlockingLock__wrappee__base( long nodeId, LockType lockType, DatabaseImpl database) throws DatabaseException { return lockInternal(nodeId,lockType,true,database); }

	 public LockResult nonBlockingLock( long nodeId, LockType lockType, DatabaseImpl database) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	nonBlockingLock__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void releaseLock__wrappee__base( long nodeId) throws DatabaseException { lockManager.release(nodeId,this); }

	 public void releaseLock( long nodeId) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	releaseLock__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void demoteLock__wrappee__base( long nodeId) throws DatabaseException { lockManager.demote(nodeId,this); }

	 public void demoteLock( long nodeId) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	demoteLock__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract boolean isTransactional__wrappee__base();

	 public abstract boolean isTransactional();{ t.in(Thread.currentThread().getStackTrace()[1].toString());	isTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract boolean isSerializableIsolation__wrappee__base();

	 public abstract boolean isSerializableIsolation();{ t.in(Thread.currentThread().getStackTrace()[1].toString());	isSerializableIsolation__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract boolean isReadCommittedIsolation__wrappee__base();

	 public abstract boolean isReadCommittedIsolation();{ t.in(Thread.currentThread().getStackTrace()[1].toString());	isReadCommittedIsolation__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract Txn getTxnLocker__wrappee__base();

	 public abstract Txn getTxnLocker();{ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTxnLocker__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract Locker newNonTxnLocker__wrappee__base() throws DatabaseException ;

	 public abstract Locker newNonTxnLocker() throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	newNonTxnLocker__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract void releaseNonTxnLocks__wrappee__base() throws DatabaseException ;

	 public abstract void releaseNonTxnLocks() throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	releaseNonTxnLocks__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean sharesLocksWith__wrappee__base( Locker other){ if (other instanceof BuddyLocker) { BuddyLocker buddy=(BuddyLocker)other; return buddy.getBuddy() == this; } else { return false; } }

	 public boolean sharesLocksWith( Locker other){ t.in(Thread.currentThread().getStackTrace()[1].toString());	sharesLocksWith__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract void operationEnd__wrappee__base() throws DatabaseException ;

	 public abstract void operationEnd() throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	operationEnd__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract void operationEnd__wrappee__base( boolean operationOK) throws DatabaseException ;

	 public abstract void operationEnd( boolean operationOK) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	operationEnd__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract void setHandleLockOwner__wrappee__base( boolean operationOK, Database dbHandle, boolean dbIsClosing) throws DatabaseException ;

	 public abstract void setHandleLockOwner( boolean operationOK, Database dbHandle, boolean dbIsClosing) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	setHandleLockOwner__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void operationEnd__wrappee__base( OperationStatus status) throws DatabaseException { operationEnd(status == OperationStatus.SUCCESS); }

	 public void operationEnd( OperationStatus status) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	operationEnd__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract void registerCursor__wrappee__base( CursorImpl cursor) throws DatabaseException ;

	 public abstract void registerCursor( CursorImpl cursor) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	registerCursor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract void unRegisterCursor__wrappee__base( CursorImpl cursor) throws DatabaseException ;

	 public abstract void unRegisterCursor( CursorImpl cursor) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	unRegisterCursor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract long getAbortLsn__wrappee__base( long nodeId) throws DatabaseException ;

	 public abstract long getAbortLsn( long nodeId) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	getAbortLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract WriteLockInfo getWriteLockInfo__wrappee__base( long nodeId) throws DatabaseException ;

	 public abstract WriteLockInfo getWriteLockInfo( long nodeId) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	getWriteLockInfo__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 abstract void addLock__wrappee__base( Long nodeId, Lock lock, LockType type, LockGrantType grantStatus) throws DatabaseException ;

	 abstract void addLock( Long nodeId, Lock lock, LockType type, LockGrantType grantStatus) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	addLock__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract boolean createdNode__wrappee__base( long nodeId) throws DatabaseException ;

	 public abstract boolean createdNode( long nodeId) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	createdNode__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 abstract void removeLock__wrappee__base( long nodeId, Lock lock) throws DatabaseException ;

	 abstract void removeLock( long nodeId, Lock lock) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	removeLock__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 abstract void moveWriteToReadLock__wrappee__base( long nodeId, Lock lock);

	 abstract void moveWriteToReadLock( long nodeId, Lock lock);{ t.in(Thread.currentThread().getStackTrace()[1].toString());	moveWriteToReadLock__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean isTimedOut__wrappee__base() throws DatabaseException { if (txnStartMillis != 0) { long diff=System.currentTimeMillis() - txnStartMillis; if (diff > txnTimeOutMillis) { return true; } } return false; }

	 boolean isTimedOut() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	isTimedOut__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getTxnTimeOut__wrappee__base(){ return txnTimeOutMillis; }

	 public long getTxnTimeOut(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTxnTimeOut__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 long getTxnStartMillis__wrappee__base(){ return txnStartMillis; }

	 long getTxnStartMillis(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTxnStartMillis__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void unregisterHandle__wrappee__base( Database dbHandle){ if (handleToHandleLockMap != null) { handleToHandleLockMap.remove(dbHandle); } }

	 void unregisterHandle( Database dbHandle){ t.in(Thread.currentThread().getStackTrace()[1].toString());	unregisterHandle__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void addToHandleMaps__wrappee__base( Long handleLockId, Database databaseHandle){ Set dbHandleSet=null; if (handleLockToHandleMap == null) { handleLockToHandleMap=new Hashtable(); handleToHandleLockMap=new Hashtable(); } else { dbHandleSet=(Set)handleLockToHandleMap.get(handleLockId); } if (dbHandleSet == null) { dbHandleSet=new HashSet(); handleLockToHandleMap.put(handleLockId,dbHandleSet); } dbHandleSet.add(databaseHandle); handleToHandleLockMap.put(databaseHandle,handleLockId); }

	 public void addToHandleMaps( Long handleLockId, Database databaseHandle){ t.in(Thread.currentThread().getStackTrace()[1].toString());	addToHandleMaps__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isHandleLockTransferrable__wrappee__base(){ return true; }

	 public boolean isHandleLockTransferrable(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isHandleLockTransferrable__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void transferHandleLockToHandle__wrappee__base( Database dbHandle) throws DatabaseException { Locker holderTxn=new BasicLocker(envImpl); transferHandleLock(dbHandle,holderTxn,true); }

	 void transferHandleLockToHandle( Database dbHandle) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	transferHandleLockToHandle__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void transferHandleLock__wrappee__base( Database dbHandle, Locker destLocker, boolean demoteToRead) throws DatabaseException { if (DbInternal.dbGetDatabaseImpl(dbHandle) != null) { Long handleLockId=(Long)handleToHandleLockMap.get(dbHandle); if (handleLockId != null) { long nodeId=handleLockId.longValue(); lockManager.transfer(nodeId,this,destLocker,demoteToRead); destLocker.addToHandleMaps(handleLockId,dbHandle); Set dbHandleSet=(Set)handleLockToHandleMap.get(handleLockId); Iterator iter=dbHandleSet.iterator(); while (iter.hasNext()) { if (((Database)iter.next()) == dbHandle) { iter.remove(); break; } } if (dbHandleSet.size() == 0) { handleLockToHandleMap.remove(handleLockId); } DbInternal.dbSetHandleLocker(dbHandle,destLocker); } } }

	 public void transferHandleLock( Database dbHandle, Locker destLocker, boolean demoteToRead) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	transferHandleLock__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void rememberHandleWriteLock__wrappee__base( Long lockId){ }

	 protected void rememberHandleWriteLock( Long lockId){ t.in(Thread.currentThread().getStackTrace()[1].toString());	rememberHandleWriteLock__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String toString__wrappee__base(){ String className=getClass().getName(); className=className.substring(className.lastIndexOf('.') + 1); return Long.toString(id) + "_" + ((thread == null) ? "" : thread.getName())+ "_"+ className; }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void dumpLockTable__wrappee__base() throws DatabaseException { lockManager.dump(); }

	 public void dumpLockTable() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLockTable__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
