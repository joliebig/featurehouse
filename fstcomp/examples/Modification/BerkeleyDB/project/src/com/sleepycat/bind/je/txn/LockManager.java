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

	
static  class  LockAttemptResult {
		 boolean success;

		 Lock useLock;

		 LockGrantType lockGrant;

		 LockAttemptResult( Lock useLock, LockGrantType lockGrant, boolean success){ this.useLock=useLock; this.lockGrant=lockGrant; this.success=success; }


	}

	 public void envConfigUpdate__wrappee__base( DbConfigManager configMgr) throws DatabaseException { LockInfo.setDeadlockStackTrace(configMgr.getBoolean(EnvironmentParams.TXN_DEADLOCK_STACK_TRACE)); setLockTableDump(configMgr.getBoolean(EnvironmentParams.TXN_DUMPLOCKS)); }

	 public void envConfigUpdate( DbConfigManager configMgr) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	envConfigUpdate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 static void setLockTableDump__wrappee__base( boolean enable){ lockTableDump=enable; }

	 static void setLockTableDump( boolean enable){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setLockTableDump__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected int getLockTableIndex__wrappee__base( Long nodeId){ return ((int)nodeId.longValue()) % nLockTables; }

	 protected int getLockTableIndex( Long nodeId){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLockTableIndex__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected int getLockTableIndex__wrappee__base( long nodeId){ return ((int)nodeId) % nLockTables; }

	 protected int getLockTableIndex( long nodeId){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLockTableIndex__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LockGrantType lock__wrappee__base( long nodeId, Locker locker, LockType type, long timeout, boolean nonBlockingRequest, DatabaseImpl database) throws DeadlockException, DatabaseException { assert timeout >= 0;
synchronized (locker) { Long nid=new Long(nodeId); LockAttemptResult result=attemptLock(nid,locker,type,nonBlockingRequest); if (result.success || result.lockGrant == LockGrantType.DENIED) { return result.lockGrant; } this.hook772(nonBlockingRequest); assert !nonBlockingRequest; try { boolean doWait=true; if (locker.isTimedOut()) { if (validateOwnership(nid,locker,type,true,memoryBudget)) { doWait=false; } else { String errMsg=makeTimeoutMsg("Transaction",locker,nodeId,type,result.lockGrant,result.useLock,locker.getTxnTimeOut(),locker.getTxnStartMillis(),System.currentTimeMillis(),database); throw new DeadlockException(errMsg); } } boolean keepTime=(timeout > 0); long startTime=(keepTime ? System.currentTimeMillis() : 0); while (doWait) { locker.setWaitingFor(result.useLock); try { locker.wait(timeout); } catch ( InterruptedException IE) { throw new RunRecoveryException(envImpl,IE); } boolean lockerTimedOut=locker.isTimedOut(); long now=System.currentTimeMillis(); boolean thisLockTimedOut=(keepTime && (now - startTime > timeout)); boolean isRestart=(result.lockGrant == LockGrantType.WAIT_RESTART); if (validateOwnership(nid,locker,type,lockerTimedOut || thisLockTimedOut || isRestart,memoryBudget)) { break; } else { if (isRestart) { throw rangeRestartException; } if (thisLockTimedOut) { locker.setOnlyAbortable(); String errMsg=makeTimeoutMsg("Lock",locker,nodeId,type,result.lockGrant,result.useLock,timeout,startTime,now,database); throw new DeadlockException(errMsg); } if (lockerTimedOut) { locker.setOnlyAbortable(); String errMsg=makeTimeoutMsg("Transaction",locker,nodeId,type,result.lockGrant,result.useLock,locker.getTxnTimeOut(),locker.getTxnStartMillis(),now,database); throw new DeadlockException(errMsg); } } } } finally { locker.setWaitingFor(null); assert EnvironmentImpl.maybeForceYield(); } locker.addLock(nid,result.useLock,type,result.lockGrant); return result.lockGrant; } }

	 public LockGrantType lock( long nodeId, Locker locker, LockType type, long timeout, boolean nonBlockingRequest, DatabaseImpl database) throws DeadlockException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	lock__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 abstract protected LockAttemptResult attemptLock__wrappee__base( Long nodeId, Locker locker, LockType type, boolean nonBlockingRequest) throws DatabaseException ;

	 abstract protected LockAttemptResult attemptLock( Long nodeId, Locker locker, LockType type, boolean nonBlockingRequest) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	attemptLock__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected LockAttemptResult attemptLockInternal__wrappee__base( Long nodeId, Locker locker, LockType type, boolean nonBlockingRequest, int lockTableIndex) throws DatabaseException { Map lockTable=lockTables[lockTableIndex]; Lock useLock=(Lock)lockTable.get(nodeId); if (useLock == null) { useLock=new Lock(nodeId); lockTable.put(nodeId,useLock); this.hook780(lockTableIndex); } LockGrantType lockGrant=useLock.lock(type,locker,nonBlockingRequest,memoryBudget,lockTableIndex); boolean success=false; if ((lockGrant == LockGrantType.NEW) || (lockGrant == LockGrantType.PROMOTION)) { locker.addLock(nodeId,useLock,type,lockGrant); success=true; } else if (lockGrant == LockGrantType.EXISTING) { success=true; } else if (lockGrant == LockGrantType.DENIED) { } else { this.hook775(); } return new LockAttemptResult(useLock,lockGrant,success); }

	 protected LockAttemptResult attemptLockInternal( Long nodeId, Locker locker, LockType type, boolean nonBlockingRequest, int lockTableIndex) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	attemptLockInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected abstract String makeTimeoutMsg__wrappee__base( String lockOrTxn, Locker locker, long nodeId, LockType type, LockGrantType grantType, Lock useLock, long timeout, long start, long now, DatabaseImpl database) throws DatabaseException ;

	 protected abstract String makeTimeoutMsg( String lockOrTxn, Locker locker, long nodeId, LockType type, LockGrantType grantType, Lock useLock, long timeout, long start, long now, DatabaseImpl database) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	makeTimeoutMsg__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected String makeTimeoutMsgInternal__wrappee__base( String lockOrTxn, Locker locker, long nodeId, LockType type, LockGrantType grantType, Lock useLock, long timeout, long start, long now, DatabaseImpl database){ if (lockTableDump) { System.out.println("++++++++++ begin lock table dump ++++++++++"); for (int i=0; i < nLockTables; i++) { StringBuffer sb=new StringBuffer(); dumpToStringNoLatch(sb,i); System.out.println(sb.toString()); } System.out.println("++++++++++ end lock table dump ++++++++++"); } StringBuffer sb=new StringBuffer(); sb.append(lockOrTxn); sb.append(" expired. Locker ").append(locker); sb.append(": waited for lock"); if (database != null) { sb.append(" on database=").append(database.getDebugName()); } sb.append(" node=").append(nodeId); sb.append(" type=").append(type); sb.append(" grant=").append(grantType); sb.append(" timeoutMillis=").append(timeout); sb.append(" startTime=").append(start); sb.append(" endTime=").append(now); sb.append("\nOwners: ").append(useLock.getOwnersClone()); sb.append("\nWaiters: ").append(useLock.getWaitersListClone()).append("\n"); StringBuffer deadlockInfo=findDeadlock(useLock,locker); if (deadlockInfo != null) { sb.append(deadlockInfo); } return sb.toString(); }

	 protected String makeTimeoutMsgInternal( String lockOrTxn, Locker locker, long nodeId, LockType type, LockGrantType grantType, Lock useLock, long timeout, long start, long now, DatabaseImpl database){ t.in(Thread.currentThread().getStackTrace()[1].toString());	makeTimeoutMsgInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean release__wrappee__base( long nodeId, Locker locker) throws DatabaseException { return release(nodeId,null,locker,true); }

	 boolean release( long nodeId, Locker locker) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	release__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean release__wrappee__base( Lock lock, Locker locker) throws DatabaseException { return release(-1,lock,locker,false); }

	 boolean release( Lock lock, Locker locker) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	release__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private boolean release__wrappee__base( long nodeId, Lock lock, Locker locker, boolean removeFromLocker) throws DatabaseException {
synchronized (locker) { Set newOwners=releaseAndFindNotifyTargets(nodeId,lock,locker,removeFromLocker); if (newOwners == null) { return false; } if (newOwners.size() > 0) { Iterator iter=newOwners.iterator(); while (iter.hasNext()) { Locker lockerToNotify=(Locker)iter.next();
synchronized (lockerToNotify) { lockerToNotify.notifyAll(); } assert EnvironmentImpl.maybeForceYield(); } } return true; } }

	 private boolean release( long nodeId, Lock lock, Locker locker, boolean removeFromLocker) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	release__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected abstract Set releaseAndFindNotifyTargets__wrappee__base( long nodeId, Lock lock, Locker locker, boolean removeFromLocker) throws DatabaseException ;

	 protected abstract Set releaseAndFindNotifyTargets( long nodeId, Lock lock, Locker locker, boolean removeFromLocker) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	releaseAndFindNotifyTargets__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected Set releaseAndFindNotifyTargetsInternal__wrappee__base( long nodeId, Lock lock, Locker locker, boolean removeFromLocker, int lockTableIndex) throws DatabaseException { Lock useLock=lock; Map lockTable=lockTables[lockTableIndex]; if (useLock == null) { useLock=(Lock)lockTable.get(new Long(nodeId)); } if (useLock == null) { return null; } Set lockersToNotify=useLock.release(locker,memoryBudget,lockTableIndex); if (lockersToNotify == null) { return null; } if (removeFromLocker) { assert nodeId != -1; locker.removeLock(nodeId,useLock); } if ((useLock.nWaiters() == 0) && (useLock.nOwners() == 0)) { lockTables[lockTableIndex].remove(useLock.getNodeId()); this.hook781(lockTableIndex); } return lockersToNotify; }

	 protected Set releaseAndFindNotifyTargetsInternal( long nodeId, Lock lock, Locker locker, boolean removeFromLocker, int lockTableIndex) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	releaseAndFindNotifyTargetsInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 abstract void transfer__wrappee__base( long nodeId, Locker owningLocker, Locker destLocker, boolean demoteToRead) throws DatabaseException ;

	 abstract void transfer( long nodeId, Locker owningLocker, Locker destLocker, boolean demoteToRead) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	transfer__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void transferInternal__wrappee__base( long nodeId, Locker owningLocker, Locker destLocker, boolean demoteToRead, int lockTableIndex) throws DatabaseException { Map lockTable=lockTables[lockTableIndex]; Lock useLock=(Lock)lockTable.get(new Long(nodeId)); assert useLock != null : "Transfer, lock " + nodeId + " was null"; if (demoteToRead) { useLock.demote(owningLocker); } useLock.transfer(owningLocker,destLocker,memoryBudget,lockTableIndex); owningLocker.removeLock(nodeId,useLock); }

	 protected void transferInternal( long nodeId, Locker owningLocker, Locker destLocker, boolean demoteToRead, int lockTableIndex) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	transferInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 abstract void transferMultiple__wrappee__base( long nodeId, Locker owningLocker, Locker[] destLockers) throws DatabaseException ;

	 abstract void transferMultiple( long nodeId, Locker owningLocker, Locker[] destLockers) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	transferMultiple__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void transferMultipleInternal__wrappee__base( long nodeId, Locker owningLocker, Locker[] destLockers, int lockTableIndex) throws DatabaseException { Map lockTable=lockTables[lockTableIndex]; Lock useLock=(Lock)lockTable.get(new Long(nodeId)); assert useLock != null : "Transfer, lock " + nodeId + " was null"; useLock.demote(owningLocker); useLock.transferMultiple(owningLocker,destLockers,memoryBudget,lockTableIndex); owningLocker.removeLock(nodeId,useLock); }

	 protected void transferMultipleInternal( long nodeId, Locker owningLocker, Locker[] destLockers, int lockTableIndex) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	transferMultipleInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 abstract void demote__wrappee__base( long nodeId, Locker locker) throws DatabaseException ;

	 abstract void demote( long nodeId, Locker locker) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	demote__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void demoteInternal__wrappee__base( long nodeId, Locker locker, int lockTableIndex) throws DatabaseException { Map lockTable=lockTables[lockTableIndex]; Lock useLock=(Lock)lockTable.get(new Long(nodeId)); useLock.demote(locker); locker.moveWriteToReadLock(nodeId,useLock); }

	 protected void demoteInternal( long nodeId, Locker locker, int lockTableIndex) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	demoteInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 abstract boolean isLocked__wrappee__base( Long nodeId) throws DatabaseException ;

	 abstract boolean isLocked( Long nodeId) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	isLocked__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected boolean isLockedInternal__wrappee__base( Long nodeId, int lockTableIndex){ Map lockTable=lockTables[lockTableIndex]; Lock entry=(Lock)lockTable.get(nodeId); if (entry == null) { return false; } return entry.nOwners() != 0; }

	 protected boolean isLockedInternal( Long nodeId, int lockTableIndex){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isLockedInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 abstract boolean isOwner__wrappee__base( Long nodeId, Locker locker, LockType type) throws DatabaseException ;

	 abstract boolean isOwner( Long nodeId, Locker locker, LockType type) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	isOwner__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected boolean isOwnerInternal__wrappee__base( Long nodeId, Locker locker, LockType type, int lockTableIndex){ Map lockTable=lockTables[lockTableIndex]; Lock entry=(Lock)lockTable.get(nodeId); if (entry == null) { return false; } return entry.isOwner(locker,type); }

	 protected boolean isOwnerInternal( Long nodeId, Locker locker, LockType type, int lockTableIndex){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isOwnerInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 abstract boolean isWaiter__wrappee__base( Long nodeId, Locker locker) throws DatabaseException ;

	 abstract boolean isWaiter( Long nodeId, Locker locker) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	isWaiter__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected boolean isWaiterInternal__wrappee__base( Long nodeId, Locker locker, int lockTableIndex){ Map lockTable=lockTables[lockTableIndex]; Lock entry=(Lock)lockTable.get(nodeId); if (entry == null) { return false; } return entry.isWaiter(locker); }

	 protected boolean isWaiterInternal( Long nodeId, Locker locker, int lockTableIndex){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isWaiterInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 abstract int nWaiters__wrappee__base( Long nodeId) throws DatabaseException ;

	 abstract int nWaiters( Long nodeId) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	nWaiters__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected int nWaitersInternal__wrappee__base( Long nodeId, int lockTableIndex){ Map lockTable=lockTables[lockTableIndex]; Lock entry=(Lock)lockTable.get(nodeId); if (entry == null) { return -1; } return entry.nWaiters(); }

	 protected int nWaitersInternal( Long nodeId, int lockTableIndex){ t.in(Thread.currentThread().getStackTrace()[1].toString());	nWaitersInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 abstract int nOwners__wrappee__base( Long nodeId) throws DatabaseException ;

	 abstract int nOwners( Long nodeId) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	nOwners__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected int nOwnersInternal__wrappee__base( Long nodeId, int lockTableIndex){ Map lockTable=lockTables[lockTableIndex]; Lock entry=(Lock)lockTable.get(nodeId); if (entry == null) { return -1; } return entry.nOwners(); }

	 protected int nOwnersInternal( Long nodeId, int lockTableIndex){ t.in(Thread.currentThread().getStackTrace()[1].toString());	nOwnersInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 abstract Locker getWriteOwnerLocker__wrappee__base( Long nodeId) throws DatabaseException ;

	 abstract Locker getWriteOwnerLocker( Long nodeId) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	getWriteOwnerLocker__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected Locker getWriteOwnerLockerInternal__wrappee__base( Long nodeId, int lockTableIndex) throws DatabaseException { Map lockTable=lockTables[lockTableIndex]; Lock lock=(Lock)lockTable.get(nodeId); if (lock == null) { return null; } else if (lock.nOwners() > 1) { return null; } else { return lock.getWriteOwnerLocker(); } }

	 protected Locker getWriteOwnerLockerInternal( Long nodeId, int lockTableIndex) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getWriteOwnerLockerInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 abstract protected boolean validateOwnership__wrappee__base( Long nodeId, Locker locker, LockType type, boolean flushFromWaiters, MemoryBudget mb) throws DatabaseException ;

	 abstract protected boolean validateOwnership( Long nodeId, Locker locker, LockType type, boolean flushFromWaiters, MemoryBudget mb) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	validateOwnership__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected boolean validateOwnershipInternal__wrappee__base( Long nodeId, Locker locker, LockType type, boolean flushFromWaiters, MemoryBudget mb, int lockTableIndex) throws DatabaseException { if (isOwnerInternal(nodeId,locker,type,lockTableIndex)) { return true; } if (flushFromWaiters) { Lock entry=(Lock)lockTables[lockTableIndex].get(nodeId); if (entry != null) { entry.flushWaiter(locker,mb,lockTableIndex); } } return false; }

	 protected boolean validateOwnershipInternal( Long nodeId, Locker locker, LockType type, boolean flushFromWaiters, MemoryBudget mb, int lockTableIndex) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	validateOwnershipInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 abstract protected void dumpLockTable__wrappee__base( LockStats stats) throws DatabaseException ;

	 abstract protected void dumpLockTable( LockStats stats) throws DatabaseException ;{ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLockTable__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void dumpLockTableInternal__wrappee__base( LockStats stats, int i){ Map lockTable=lockTables[i]; this.hook776(stats,lockTable); Iterator iter=lockTable.values().iterator(); while (iter.hasNext()) { Lock lock=(Lock)iter.next(); this.hook777(stats,lock); Iterator ownerIter=lock.getOwnersClone().iterator(); while (ownerIter.hasNext()) { LockInfo info=(LockInfo)ownerIter.next(); this.hook778(stats,info); } } }

	 protected void dumpLockTableInternal( LockStats stats, int i){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpLockTableInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void dump__wrappee__base() throws DatabaseException { System.out.println(dumpToString()); }

	 public void dump() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	dump__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String dumpToString__wrappee__base() throws DatabaseException { StringBuffer sb=new StringBuffer(); for (int i=0; i < nLockTables; i++) { this.hook773(sb,i); } return sb.toString(); }

	 public String dumpToString() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpToString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void dumpToStringNoLatch__wrappee__base( StringBuffer sb, int whichTable){ Map lockTable=lockTables[whichTable]; Iterator entries=lockTable.entrySet().iterator(); while (entries.hasNext()) { Map.Entry entry=(Map.Entry)entries.next(); Long nid=(Long)entry.getKey(); Lock lock=(Lock)entry.getValue(); sb.append("---- Node Id: ").append(nid).append("----\n"); sb.append(lock); sb.append('\n'); } }

	 private void dumpToStringNoLatch( StringBuffer sb, int whichTable){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpToStringNoLatch__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private StringBuffer findDeadlock__wrappee__base( Lock lock, Locker rootLocker){ Set ownerSet=new HashSet(); ownerSet.add(rootLocker); StringBuffer ret=findDeadlock1(ownerSet,lock,rootLocker); if (ret != null) { return ret; } else { return null; } }

	 private StringBuffer findDeadlock( Lock lock, Locker rootLocker){ t.in(Thread.currentThread().getStackTrace()[1].toString());	findDeadlock__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private StringBuffer findDeadlock1__wrappee__base( Set ownerSet, Lock lock, Locker rootLocker){ Iterator ownerIter=lock.getOwnersClone().iterator(); while (ownerIter.hasNext()) { LockInfo info=(LockInfo)ownerIter.next(); Locker locker=info.getLocker(); Lock waitsFor=locker.getWaitingFor(); if (ownerSet.contains(locker) || locker == rootLocker) { StringBuffer ret=new StringBuffer(); ret.append("Transaction ").append(locker.toString()); ret.append(" owns ").append(lock.getNodeId()); ret.append(" ").append(info).append("\n"); ret.append("Transaction ").append(locker.toString()); ret.append(" waits for "); if (waitsFor == null) { ret.append(" nothing"); } else { ret.append(" node "); ret.append(waitsFor.getNodeId()); } ret.append("\n"); return ret; } if (waitsFor != null) { ownerSet.add(locker); StringBuffer sb=findDeadlock1(ownerSet,waitsFor,rootLocker); if (sb != null) { String waitInfo="Transaction " + locker + " waits for node "+ waitsFor.getNodeId()+ "\n"; sb.insert(0,waitInfo); return sb; } ownerSet.remove(locker); } } return null; }

	 private StringBuffer findDeadlock1( Set ownerSet, Lock lock, Locker rootLocker){ t.in(Thread.currentThread().getStackTrace()[1].toString());	findDeadlock1__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook770__wrappee__base() throws DatabaseException { }

	 protected void hook770() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook770__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook771__wrappee__base( EnvironmentImpl envImpl, int i) throws DatabaseException { }

	 protected void hook771( EnvironmentImpl envImpl, int i) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook771__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook772__wrappee__base( boolean nonBlockingRequest) throws DeadlockException, DatabaseException { }

	 protected void hook772( boolean nonBlockingRequest) throws DeadlockException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook772__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook773__wrappee__base( StringBuffer sb, int i) throws DatabaseException { dumpToStringNoLatch(sb,i); }

	 protected void hook773( StringBuffer sb, int i) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook773__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook774__wrappee__base() throws DatabaseException { }

	 protected void hook774() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook774__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook775__wrappee__base() throws DatabaseException { }

	 protected void hook775() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook775__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook776__wrappee__base( LockStats stats, Map lockTable){ }

	 protected void hook776( LockStats stats, Map lockTable){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook776__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook777__wrappee__base( LockStats stats, Lock lock){ }

	 protected void hook777( LockStats stats, Lock lock){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook777__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook778__wrappee__base( LockStats stats, LockInfo info){ }

	 protected void hook778( LockStats stats, LockInfo info){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook778__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook779__wrappee__base( DbConfigManager configMgr) throws DatabaseException { }

	 protected void hook779( DbConfigManager configMgr) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook779__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook780__wrappee__base( int lockTableIndex) throws DatabaseException { }

	 protected void hook780( int lockTableIndex) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook780__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook781__wrappee__base( int lockTableIndex) throws DatabaseException { }

	 protected void hook781( int lockTableIndex) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook781__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
