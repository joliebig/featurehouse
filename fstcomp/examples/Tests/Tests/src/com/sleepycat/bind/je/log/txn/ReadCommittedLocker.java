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

	 public Locker newNonTxnLocker() throws DatabaseException { return new ReadCommittedLocker(envImpl,getBuddy().newNonTxnLocker()); }

	 LockResult lockInternal( long nodeId, LockType lockType, boolean noWait, DatabaseImpl database) throws DatabaseException { if (lockType.isWriteLock()) { return getBuddy().lockInternal(nodeId,lockType,noWait,database); } else { return super.lockInternal(nodeId,lockType,noWait,database); } }

	 public void releaseLock( long nodeId) throws DatabaseException { if (!lockManager.release(nodeId,this)) { lockManager.release(nodeId,getBuddy()); } }

	 public boolean createdNode( long nodeId) throws DatabaseException { return getBuddy().createdNode(nodeId); }

	 public long getAbortLsn( long nodeId) throws DatabaseException { return getBuddy().getAbortLsn(nodeId); }

	 public WriteLockInfo getWriteLockInfo( long nodeId) throws DatabaseException { return getBuddy().getWriteLockInfo(nodeId); }

	 public void registerCursor( CursorImpl cursor) throws DatabaseException { getBuddy().registerCursor(cursor); }

	 public void unRegisterCursor( CursorImpl cursor) throws DatabaseException { getBuddy().unRegisterCursor(cursor); }

	 public boolean isTransactional(){ return true; }

	 public boolean isReadCommittedIsolation(){ return true; }


}
