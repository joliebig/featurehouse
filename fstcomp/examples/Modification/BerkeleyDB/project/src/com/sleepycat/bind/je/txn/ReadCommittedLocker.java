package com.sleepycat.je.txn; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.CursorImpl; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.tree.BIN; 
import com.sleepycat.je.tree.Key; 
import de.ovgu.cide.jakutil.*; 
public  class  ReadCommittedLocker  extends BuddyLocker {
	 public ReadCommittedLocker( EnvironmentImpl env, Locker buddy) throws DatabaseException { super(env,(buddy instanceof ReadCommittedLocker) ? ((ReadCommittedLocker)buddy).getBuddy() : buddy); assert getBuddy().isTransactional(); }

	 public Locker newNonTxnLocker__wrappee__base() throws DatabaseException { return new ReadCommittedLocker(envImpl,getBuddy().newNonTxnLocker()); }

	 public Locker newNonTxnLocker() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	newNonTxnLocker__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 LockResult lockInternal__wrappee__base( long nodeId, LockType lockType, boolean noWait, DatabaseImpl database) throws DatabaseException { if (lockType.isWriteLock()) { return getBuddy().lockInternal(nodeId,lockType,noWait,database); } else { return super.lockInternal(nodeId,lockType,noWait,database); } }

	 LockResult lockInternal( long nodeId, LockType lockType, boolean noWait, DatabaseImpl database) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	lockInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void releaseLock__wrappee__base( long nodeId) throws DatabaseException { if (!lockManager.release(nodeId,this)) { lockManager.release(nodeId,getBuddy()); } }

	 public void releaseLock( long nodeId) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	releaseLock__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean createdNode__wrappee__base( long nodeId) throws DatabaseException { return getBuddy().createdNode(nodeId); }

	 public boolean createdNode( long nodeId) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	createdNode__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getAbortLsn__wrappee__base( long nodeId) throws DatabaseException { return getBuddy().getAbortLsn(nodeId); }

	 public long getAbortLsn( long nodeId) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getAbortLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public WriteLockInfo getWriteLockInfo__wrappee__base( long nodeId) throws DatabaseException { return getBuddy().getWriteLockInfo(nodeId); }

	 public WriteLockInfo getWriteLockInfo( long nodeId) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getWriteLockInfo__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void registerCursor__wrappee__base( CursorImpl cursor) throws DatabaseException { getBuddy().registerCursor(cursor); }

	 public void registerCursor( CursorImpl cursor) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	registerCursor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void unRegisterCursor__wrappee__base( CursorImpl cursor) throws DatabaseException { getBuddy().unRegisterCursor(cursor); }

	 public void unRegisterCursor( CursorImpl cursor) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	unRegisterCursor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isTransactional__wrappee__base(){ return true; }

	 public boolean isTransactional(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isReadCommittedIsolation__wrappee__base(){ return true; }

	 public boolean isReadCommittedIsolation(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isReadCommittedIsolation__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
