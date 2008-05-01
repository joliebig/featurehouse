package com.sleepycat.je.txn; 
import java.util.HashMap; 
import java.util.HashSet; 
import java.util.Iterator; 
import java.util.Map; 
import java.util.Set; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.DeadlockException; 
import com.sleepycat.je.LockStats; 
import com.sleepycat.je.RunRecoveryException; 
import com.sleepycat.je.config.EnvironmentParams; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.dbi.DbConfigManager; 
import com.sleepycat.je.dbi.EnvConfigObserver; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.dbi.MemoryBudget; 
import com.sleepycat.je.dbi.RangeRestartException; 
import de.ovgu.cide.jakutil.*; 
public abstract  class  LockManager  implements EnvConfigObserver {
	 protected int nLockTables=1;

	 private Map[] lockTables;

	 private EnvironmentImpl envImpl;

	 private MemoryBudget memoryBudget;

	 private static RangeRestartException rangeRestartException=new RangeRestartException();

	 private static boolean lockTableDump=false;

	 public LockManager( EnvironmentImpl envImpl) throws DatabaseException { DbConfigManager configMgr=envImpl.getConfigManager(); this.hook779(configMgr); lockTables=new Map[nLockTables]; this.hook770(); for (int i=0; i < nLockTables; i++) { lockTables[i]=new HashMap(); this.hook771(envImpl,i); } this.envImpl=envImpl; memoryBudget=envImpl.getMemoryBudget(); this.hook774(); envConfigUpdate(configMgr); envImpl.addConfigObserver(this); }

	 public void envConfigUpdate( DbConfigManager configMgr) throws DatabaseException { LockInfo.setDeadlockStackTrace(configMgr.getBoolean(EnvironmentParams.TXN_DEADLOCK_STACK_TRACE)); setLockTableDump(configMgr.getBoolean(EnvironmentParams.TXN_DUMPLOCKS)); }

	 static void setLockTableDump( boolean enable){ lockTableDump=enable; }

	 protected int getLockTableIndex( Long nodeId){ return ((int)nodeId.longValue()) % nLockTables; }

	 protected int getLockTableIndex( long nodeId){ return ((int)nodeId) % nLockTables; }

	 public LockGrantType lock( long nodeId, Locker locker, LockType type, long timeout, boolean nonBlockingRequest, DatabaseImpl database) throws DeadlockException, DatabaseException { assert timeout >= 0;
synchronized (locker) { Long nid=new Long(nodeId); LockAttemptResult result=attemptLock(nid,locker,type,nonBlockingRequest); if (result.success || result.lockGrant == LockGrantType.DENIED) { return result.lockGrant; } this.hook772(nonBlockingRequest); assert !nonBlockingRequest; try { boolean doWait=true; if (locker.isTimedOut()) { if (validateOwnership(nid,locker,type,true,memoryBudget)) { doWait=false; } else { String errMsg=makeTimeoutMsg("Transaction",locker,nodeId,type,result.lockGrant,result.useLock,locker.getTxnTimeOut(),locker.getTxnStartMillis(),System.currentTimeMillis(),database); throw new DeadlockException(errMsg); } } boolean keepTime=(timeout > 0); long startTime=(keepTime ? System.currentTimeMillis() : 0); while (doWait) { locker.setWaitingFor(result.useLock); try { locker.wait(timeout); } catch ( InterruptedException IE) { throw new RunRecoveryException(envImpl,IE); } boolean lockerTimedOut=locker.isTimedOut(); long now=System.currentTimeMillis(); boolean thisLockTimedOut=(keepTime && (now - startTime > timeout)); boolean isRestart=(result.lockGrant == LockGrantType.WAIT_RESTART); if (validateOwnership(nid,locker,type,lockerTimedOut || thisLockTimedOut || isRestart,memoryBudget)) { break; } else { if (isRestart) { throw rangeRestartException; } if (thisLockTimedOut) { locker.setOnlyAbortable(); String errMsg=makeTimeoutMsg("Lock",locker,nodeId,type,result.lockGrant,result.useLock,timeout,startTime,now,database); throw new DeadlockException(errMsg); } if (lockerTimedOut) { locker.setOnlyAbortable(); String errMsg=makeTimeoutMsg("Transaction",locker,nodeId,type,result.lockGrant,result.useLock,locker.getTxnTimeOut(),locker.getTxnStartMillis(),now,database); throw new DeadlockException(errMsg); } } } } finally { locker.setWaitingFor(null); assert EnvironmentImpl.maybeForceYield(); } locker.addLock(nid,result.useLock,type,result.lockGrant); return result.lockGrant; } }

	 abstract protected LockAttemptResult attemptLock( Long nodeId, Locker locker, LockType type, boolean nonBlockingRequest) throws DatabaseException ;

	 protected LockAttemptResult attemptLockInternal( Long nodeId, Locker locker, LockType type, boolean nonBlockingRequest, int lockTableIndex) throws DatabaseException { Map lockTable=lockTables[lockTableIndex]; Lock useLock=(Lock)lockTable.get(nodeId); if (useLock == null) { useLock=new Lock(nodeId); lockTable.put(nodeId,useLock); this.hook780(lockTableIndex); } LockGrantType lockGrant=useLock.lock(type,locker,nonBlockingRequest,memoryBudget,lockTableIndex); boolean success=false; if ((lockGrant == LockGrantType.NEW) || (lockGrant == LockGrantType.PROMOTION)) { locker.addLock(nodeId,useLock,type,lockGrant); success=true; } else if (lockGrant == LockGrantType.EXISTING) { success=true; } else if (lockGrant == LockGrantType.DENIED) { } else { this.hook775(); } return new LockAttemptResult(useLock,lockGrant,success); }

	 protected abstract String makeTimeoutMsg( String lockOrTxn, Locker locker, long nodeId, LockType type, LockGrantType grantType, Lock useLock, long timeout, long start, long now, DatabaseImpl database) throws DatabaseException ;

	 protected String makeTimeoutMsgInternal( String lockOrTxn, Locker locker, long nodeId, LockType type, LockGrantType grantType, Lock useLock, long timeout, long start, long now, DatabaseImpl database){ if (lockTableDump) { System.out.println("++++++++++ begin lock table dump ++++++++++"); for (int i=0; i < nLockTables; i++) { StringBuffer sb=new StringBuffer(); dumpToStringNoLatch(sb,i); System.out.println(sb.toString()); } System.out.println("++++++++++ end lock table dump ++++++++++"); } StringBuffer sb=new StringBuffer(); sb.append(lockOrTxn); sb.append(" expired. Locker ").append(locker); sb.append(": waited for lock"); if (database != null) { sb.append(" on database=").append(database.getDebugName()); } sb.append(" node=").append(nodeId); sb.append(" type=").append(type); sb.append(" grant=").append(grantType); sb.append(" timeoutMillis=").append(timeout); sb.append(" startTime=").append(start); sb.append(" endTime=").append(now); sb.append("\nOwners: ").append(useLock.getOwnersClone()); sb.append("\nWaiters: ").append(useLock.getWaitersListClone()).append("\n"); StringBuffer deadlockInfo=findDeadlock(useLock,locker); if (deadlockInfo != null) { sb.append(deadlockInfo); } return sb.toString(); }

	 boolean release( long nodeId, Locker locker) throws DatabaseException { return release(nodeId,null,locker,true); }

	 boolean release( Lock lock, Locker locker) throws DatabaseException { return release(-1,lock,locker,false); }

	 private boolean release( long nodeId, Lock lock, Locker locker, boolean removeFromLocker) throws DatabaseException {
synchronized (locker) { Set newOwners=releaseAndFindNotifyTargets(nodeId,lock,locker,removeFromLocker); if (newOwners == null) { return false; } if (newOwners.size() > 0) { Iterator iter=newOwners.iterator(); while (iter.hasNext()) { Locker lockerToNotify=(Locker)iter.next();
synchronized (lockerToNotify) { lockerToNotify.notifyAll(); } assert EnvironmentImpl.maybeForceYield(); } } return true; } }

	 protected abstract Set releaseAndFindNotifyTargets( long nodeId, Lock lock, Locker locker, boolean removeFromLocker) throws DatabaseException ;

	 protected Set releaseAndFindNotifyTargetsInternal( long nodeId, Lock lock, Locker locker, boolean removeFromLocker, int lockTableIndex) throws DatabaseException { Lock useLock=lock; Map lockTable=lockTables[lockTableIndex]; if (useLock == null) { useLock=(Lock)lockTable.get(new Long(nodeId)); } if (useLock == null) { return null; } Set lockersToNotify=useLock.release(locker,memoryBudget,lockTableIndex); if (lockersToNotify == null) { return null; } if (removeFromLocker) { assert nodeId != -1; locker.removeLock(nodeId,useLock); } if ((useLock.nWaiters() == 0) && (useLock.nOwners() == 0)) { lockTables[lockTableIndex].remove(useLock.getNodeId()); this.hook781(lockTableIndex); } return lockersToNotify; }

	 abstract void transfer( long nodeId, Locker owningLocker, Locker destLocker, boolean demoteToRead) throws DatabaseException ;

	 protected void transferInternal( long nodeId, Locker owningLocker, Locker destLocker, boolean demoteToRead, int lockTableIndex) throws DatabaseException { Map lockTable=lockTables[lockTableIndex]; Lock useLock=(Lock)lockTable.get(new Long(nodeId)); assert useLock != null : "Transfer, lock " + nodeId + " was null"; if (demoteToRead) { useLock.demote(owningLocker); } useLock.transfer(owningLocker,destLocker,memoryBudget,lockTableIndex); owningLocker.removeLock(nodeId,useLock); }

	 abstract void transferMultiple( long nodeId, Locker owningLocker, Locker[] destLockers) throws DatabaseException ;

	 protected void transferMultipleInternal( long nodeId, Locker owningLocker, Locker[] destLockers, int lockTableIndex) throws DatabaseException { Map lockTable=lockTables[lockTableIndex]; Lock useLock=(Lock)lockTable.get(new Long(nodeId)); assert useLock != null : "Transfer, lock " + nodeId + " was null"; useLock.demote(owningLocker); useLock.transferMultiple(owningLocker,destLockers,memoryBudget,lockTableIndex); owningLocker.removeLock(nodeId,useLock); }

	 abstract void demote( long nodeId, Locker locker) throws DatabaseException ;

	 protected void demoteInternal( long nodeId, Locker locker, int lockTableIndex) throws DatabaseException { Map lockTable=lockTables[lockTableIndex]; Lock useLock=(Lock)lockTable.get(new Long(nodeId)); useLock.demote(locker); locker.moveWriteToReadLock(nodeId,useLock); }

	 abstract boolean isLocked( Long nodeId) throws DatabaseException ;

	 protected boolean isLockedInternal( Long nodeId, int lockTableIndex){ Map lockTable=lockTables[lockTableIndex]; Lock entry=(Lock)lockTable.get(nodeId); if (entry == null) { return false; } return entry.nOwners() != 0; }

	 abstract boolean isOwner( Long nodeId, Locker locker, LockType type) throws DatabaseException ;

	 protected boolean isOwnerInternal( Long nodeId, Locker locker, LockType type, int lockTableIndex){ Map lockTable=lockTables[lockTableIndex]; Lock entry=(Lock)lockTable.get(nodeId); if (entry == null) { return false; } return entry.isOwner(locker,type); }

	 abstract boolean isWaiter( Long nodeId, Locker locker) throws DatabaseException ;

	 protected boolean isWaiterInternal( Long nodeId, Locker locker, int lockTableIndex){ Map lockTable=lockTables[lockTableIndex]; Lock entry=(Lock)lockTable.get(nodeId); if (entry == null) { return false; } return entry.isWaiter(locker); }

	 abstract int nWaiters( Long nodeId) throws DatabaseException ;

	 protected int nWaitersInternal( Long nodeId, int lockTableIndex){ Map lockTable=lockTables[lockTableIndex]; Lock entry=(Lock)lockTable.get(nodeId); if (entry == null) { return -1; } return entry.nWaiters(); }

	 abstract int nOwners( Long nodeId) throws DatabaseException ;

	 protected int nOwnersInternal( Long nodeId, int lockTableIndex){ Map lockTable=lockTables[lockTableIndex]; Lock entry=(Lock)lockTable.get(nodeId); if (entry == null) { return -1; } return entry.nOwners(); }

	 abstract Locker getWriteOwnerLocker( Long nodeId) throws DatabaseException ;

	 protected Locker getWriteOwnerLockerInternal( Long nodeId, int lockTableIndex) throws DatabaseException { Map lockTable=lockTables[lockTableIndex]; Lock lock=(Lock)lockTable.get(nodeId); if (lock == null) { return null; } else if (lock.nOwners() > 1) { return null; } else { return lock.getWriteOwnerLocker(); } }

	 abstract protected boolean validateOwnership( Long nodeId, Locker locker, LockType type, boolean flushFromWaiters, MemoryBudget mb) throws DatabaseException ;

	 protected boolean validateOwnershipInternal( Long nodeId, Locker locker, LockType type, boolean flushFromWaiters, MemoryBudget mb, int lockTableIndex) throws DatabaseException { if (isOwnerInternal(nodeId,locker,type,lockTableIndex)) { return true; } if (flushFromWaiters) { Lock entry=(Lock)lockTables[lockTableIndex].get(nodeId); if (entry != null) { entry.flushWaiter(locker,mb,lockTableIndex); } } return false; }

	 abstract protected void dumpLockTable( LockStats stats) throws DatabaseException ;

	 protected void dumpLockTableInternal( LockStats stats, int i){ Map lockTable=lockTables[i]; this.hook776(stats,lockTable); Iterator iter=lockTable.values().iterator(); while (iter.hasNext()) { Lock lock=(Lock)iter.next(); this.hook777(stats,lock); Iterator ownerIter=lock.getOwnersClone().iterator(); while (ownerIter.hasNext()) { LockInfo info=(LockInfo)ownerIter.next(); this.hook778(stats,info); } } }

	 public void dump() throws DatabaseException { System.out.println(dumpToString()); }

	 public String dumpToString() throws DatabaseException { StringBuffer sb=new StringBuffer(); for (int i=0; i < nLockTables; i++) { this.hook773(sb,i); } return sb.toString(); }

	 private void dumpToStringNoLatch( StringBuffer sb, int whichTable){ Map lockTable=lockTables[whichTable]; Iterator entries=lockTable.entrySet().iterator(); while (entries.hasNext()) { Map.Entry entry=(Map.Entry)entries.next(); Long nid=(Long)entry.getKey(); Lock lock=(Lock)entry.getValue(); sb.append("---- Node Id: ").append(nid).append("----\n"); sb.append(lock); sb.append('\n'); } }

	 private StringBuffer findDeadlock( Lock lock, Locker rootLocker){ Set ownerSet=new HashSet(); ownerSet.add(rootLocker); StringBuffer ret=findDeadlock1(ownerSet,lock,rootLocker); if (ret != null) { return ret; } else { return null; } }

	 private StringBuffer findDeadlock1( Set ownerSet, Lock lock, Locker rootLocker){ Iterator ownerIter=lock.getOwnersClone().iterator(); while (ownerIter.hasNext()) { LockInfo info=(LockInfo)ownerIter.next(); Locker locker=info.getLocker(); Lock waitsFor=locker.getWaitingFor(); if (ownerSet.contains(locker) || locker == rootLocker) { StringBuffer ret=new StringBuffer(); ret.append("Transaction ").append(locker.toString()); ret.append(" owns ").append(lock.getNodeId()); ret.append(" ").append(info).append("\n"); ret.append("Transaction ").append(locker.toString()); ret.append(" waits for "); if (waitsFor == null) { ret.append(" nothing"); } else { ret.append(" node "); ret.append(waitsFor.getNodeId()); } ret.append("\n"); return ret; } if (waitsFor != null) { ownerSet.add(locker); StringBuffer sb=findDeadlock1(ownerSet,waitsFor,rootLocker); if (sb != null) { String waitInfo="Transaction " + locker + " waits for node "+ waitsFor.getNodeId()+ "\n"; sb.insert(0,waitInfo); return sb; } ownerSet.remove(locker); } } return null; }

	
static  class  LockAttemptResult {
		 boolean success;

		 Lock useLock;

		 LockGrantType lockGrant;

		 LockAttemptResult( Lock useLock, LockGrantType lockGrant, boolean success){ this.useLock=useLock; this.lockGrant=lockGrant; this.success=success; }


	}

	 protected void hook770() throws DatabaseException { }

	 protected void hook771( EnvironmentImpl envImpl, int i) throws DatabaseException { }

	 protected void hook772( boolean nonBlockingRequest) throws DeadlockException, DatabaseException { }

	 protected void hook773( StringBuffer sb, int i) throws DatabaseException { dumpToStringNoLatch(sb,i); }

	 protected void hook774() throws DatabaseException { }

	 protected void hook775() throws DatabaseException { }

	 protected void hook776( LockStats stats, Map lockTable){ }

	 protected void hook777( LockStats stats, Lock lock){ }

	 protected void hook778( LockStats stats, LockInfo info){ }

	 protected void hook779( DbConfigManager configMgr) throws DatabaseException { }

	 protected void hook780( int lockTableIndex) throws DatabaseException { }

	 protected void hook781( int lockTableIndex) throws DatabaseException { }


}
