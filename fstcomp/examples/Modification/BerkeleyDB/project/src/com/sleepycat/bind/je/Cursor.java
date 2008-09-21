package com.sleepycat.je; 
import java.util.logging.Level; 
import java.util.logging.Logger; 
import com.sleepycat.je.dbi.CursorImpl; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.dbi.GetMode; 
import com.sleepycat.je.dbi.PutMode; 
import com.sleepycat.je.dbi.RangeRestartException; 
import com.sleepycat.je.dbi.CursorImpl.KeyChangeStatus; 
import com.sleepycat.je.dbi.CursorImpl.SearchMode; 
import com.sleepycat.je.tree.BIN; 
import com.sleepycat.je.tree.DBIN; 
import com.sleepycat.je.tree.Key; 
import com.sleepycat.je.tree.LN; 
import com.sleepycat.je.tree.Node; 
import com.sleepycat.je.txn.BuddyLocker; 
import com.sleepycat.je.txn.LockType; 
import com.sleepycat.je.txn.Locker; 
import com.sleepycat.je.txn.LockerFactory; 
import com.sleepycat.je.utilint.InternalException; 
import de.ovgu.cide.jakutil.*; 
public  class  Cursor {
	 CursorImpl cursorImpl;

	 CursorConfig config;

	 private boolean updateOperationsProhibited;

	 private Database dbHandle;

	 private DatabaseImpl dbImpl;

	 private boolean readUncommittedDefault;

	 private boolean serializableIsolationDefault;

	 Cursor( Database dbHandle, Transaction txn, CursorConfig cursorConfig) throws DatabaseException { if (cursorConfig == null) { cursorConfig=CursorConfig.DEFAULT; } Locker locker=LockerFactory.getReadableLocker(dbHandle.getEnvironment(),txn,dbHandle.isTransactional(),false,cursorConfig.getReadCommitted()); init(dbHandle,dbHandle.getDatabaseImpl(),locker,dbHandle.isWritable(),cursorConfig); }

	 Cursor( Database dbHandle, Locker locker, CursorConfig cursorConfig) throws DatabaseException { if (cursorConfig == null) { cursorConfig=CursorConfig.DEFAULT; } locker=LockerFactory.getReadableLocker(dbHandle.getEnvironment(),dbHandle,locker,false,cursorConfig.getReadCommitted()); init(dbHandle,dbHandle.getDatabaseImpl(),locker,dbHandle.isWritable(),cursorConfig); }

	 Cursor( DatabaseImpl dbImpl, Locker locker, CursorConfig cursorConfig) throws DatabaseException { if (cursorConfig == null) { cursorConfig=CursorConfig.DEFAULT; } init(null,dbImpl,locker,true,cursorConfig); }

	 Cursor( Cursor cursor, boolean samePosition) throws DatabaseException { readUncommittedDefault=cursor.readUncommittedDefault; serializableIsolationDefault=cursor.serializableIsolationDefault; updateOperationsProhibited=cursor.updateOperationsProhibited; cursorImpl=cursor.cursorImpl.dup(samePosition); dbImpl=cursor.dbImpl; dbHandle=cursor.dbHandle; if (dbHandle != null) { dbHandle.addCursor(this); } config=cursor.config; }

	 private void init__wrappee__base( Database dbHandle, DatabaseImpl dbImpl, Locker locker, boolean isWritable, CursorConfig cursorConfig) throws DatabaseException { assert locker != null; assert dbImpl != null; cursorImpl=new CursorImpl(dbImpl,locker,false); readUncommittedDefault=cursorConfig.getReadUncommitted() || locker.isReadUncommittedDefault(); serializableIsolationDefault=cursorImpl.getLocker().isSerializableIsolation(); updateOperationsProhibited=(dbImpl.isTransactional() && !locker.isTransactional()) || !isWritable; this.dbImpl=dbImpl; this.dbHandle=dbHandle; if (dbHandle != null) { dbHandle.addCursor(this); } this.config=cursorConfig; this.hook36(dbImpl); }

	 private void init( Database dbHandle, DatabaseImpl dbImpl, Locker locker, boolean isWritable, CursorConfig cursorConfig) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	init__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 CursorImpl getCursorImpl__wrappee__base(){ return cursorImpl; }

	 CursorImpl getCursorImpl(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getCursorImpl__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Database getDatabase__wrappee__base(){ return dbHandle; }

	 public Database getDatabase(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDatabase__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 DatabaseImpl getDatabaseImpl__wrappee__base(){ return dbImpl; }

	 DatabaseImpl getDatabaseImpl(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDatabaseImpl__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public CursorConfig getConfig__wrappee__base(){ return config.cloneConfig(); }

	 public CursorConfig getConfig(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getConfig__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setNonCloning__wrappee__base( boolean nonCloning){ cursorImpl.setNonCloning(nonCloning); }

	 void setNonCloning( boolean nonCloning){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setNonCloning__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public synchronized void close__wrappee__base() throws DatabaseException { checkState(false); cursorImpl.close(); if (dbHandle != null) { dbHandle.removeCursor(this); } }

	 public synchronized void close() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	close__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int count__wrappee__base() throws DatabaseException { checkState(true); this.hook0(); return countInternal(null); }

	 public int count() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	count__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Cursor dup__wrappee__base( boolean samePosition) throws DatabaseException { checkState(false); return new Cursor(this,samePosition); }

	 public Cursor dup( boolean samePosition) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	dup__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus delete__wrappee__base() throws DatabaseException { checkState(true); checkUpdatesAllowed("delete"); this.hook1(); return deleteInternal(); }

	 public OperationStatus delete() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	delete__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus put__wrappee__base( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { checkState(false); DatabaseUtil.checkForNullDbt(key,"key",true); DatabaseUtil.checkForNullDbt(data,"data",true); DatabaseUtil.checkForPartialKey(key); checkUpdatesAllowed("put"); this.hook2(key,data); return putInternal(key,data,PutMode.OVERWRITE); }

	 public OperationStatus put( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	put__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus putNoOverwrite__wrappee__base( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { checkState(false); DatabaseUtil.checkForNullDbt(key,"key",true); DatabaseUtil.checkForNullDbt(data,"data",true); DatabaseUtil.checkForPartialKey(key); checkUpdatesAllowed("putNoOverwrite"); this.hook3(key,data); return putInternal(key,data,PutMode.NOOVERWRITE); }

	 public OperationStatus putNoOverwrite( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	putNoOverwrite__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus putNoDupData__wrappee__base( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { checkState(false); DatabaseUtil.checkForNullDbt(key,"key",true); DatabaseUtil.checkForNullDbt(data,"data",true); DatabaseUtil.checkForPartialKey(key); checkUpdatesAllowed("putNoDupData"); this.hook4(key,data); return putInternal(key,data,PutMode.NODUP); }

	 public OperationStatus putNoDupData( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	putNoDupData__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus putCurrent__wrappee__base( DatabaseEntry data) throws DatabaseException { checkState(true); DatabaseUtil.checkForNullDbt(data,"data",true); checkUpdatesAllowed("putCurrent"); this.hook5(data); return putInternal(null,data,PutMode.CURRENT); }

	 public OperationStatus putCurrent( DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	putCurrent__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getCurrent__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(true); checkArgsNoValRequired(key,data); this.hook6(lockMode); return getCurrentInternal(key,data,lockMode); }

	 public OperationStatus getCurrent( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getCurrent__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getFirst__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); checkArgsNoValRequired(key,data); this.hook7(lockMode); return position(key,data,lockMode,true); }

	 public OperationStatus getFirst( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getFirst__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getLast__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); checkArgsNoValRequired(key,data); this.hook8(lockMode); return position(key,data,lockMode,false); }

	 public OperationStatus getLast( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getLast__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getNext__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); checkArgsNoValRequired(key,data); this.hook9(lockMode); if (cursorImpl.isNotInitialized()) { return position(key,data,lockMode,true); } else { return retrieveNext(key,data,lockMode,GetMode.NEXT); } }

	 public OperationStatus getNext( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getNext__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getNextDup__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(true); checkArgsNoValRequired(key,data); this.hook10(lockMode); return retrieveNext(key,data,lockMode,GetMode.NEXT_DUP); }

	 public OperationStatus getNextDup( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getNextDup__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getNextNoDup__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); checkArgsNoValRequired(key,data); this.hook11(lockMode); if (cursorImpl.isNotInitialized()) { return position(key,data,lockMode,true); } else { return retrieveNext(key,data,lockMode,GetMode.NEXT_NODUP); } }

	 public OperationStatus getNextNoDup( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getNextNoDup__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getPrev__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); checkArgsNoValRequired(key,data); this.hook12(lockMode); if (cursorImpl.isNotInitialized()) { return position(key,data,lockMode,false); } else { return retrieveNext(key,data,lockMode,GetMode.PREV); } }

	 public OperationStatus getPrev( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getPrev__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getPrevDup__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(true); checkArgsNoValRequired(key,data); this.hook13(lockMode); return retrieveNext(key,data,lockMode,GetMode.PREV_DUP); }

	 public OperationStatus getPrevDup( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getPrevDup__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getPrevNoDup__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); checkArgsNoValRequired(key,data); this.hook14(lockMode); if (cursorImpl.isNotInitialized()) { return position(key,data,lockMode,false); } else { return retrieveNext(key,data,lockMode,GetMode.PREV_NODUP); } }

	 public OperationStatus getPrevNoDup( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getPrevNoDup__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getSearchKey__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); DatabaseUtil.checkForNullDbt(key,"key",true); DatabaseUtil.checkForNullDbt(data,"data",false); this.hook15(key,lockMode); return search(key,data,lockMode,SearchMode.SET); }

	 public OperationStatus getSearchKey( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getSearchKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getSearchKeyRange__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); DatabaseUtil.checkForNullDbt(key,"key",true); DatabaseUtil.checkForNullDbt(data,"data",false); this.hook16(key,lockMode); return search(key,data,lockMode,SearchMode.SET_RANGE); }

	 public OperationStatus getSearchKeyRange( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getSearchKeyRange__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getSearchBoth__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); checkArgsValRequired(key,data); this.hook17(key,data,lockMode); return search(key,data,lockMode,SearchMode.BOTH); }

	 public OperationStatus getSearchBoth( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getSearchBoth__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getSearchBothRange__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); checkArgsValRequired(key,data); this.hook18(key,data,lockMode); return search(key,data,lockMode,SearchMode.BOTH_RANGE); }

	 public OperationStatus getSearchBothRange( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getSearchBothRange__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 int countInternal__wrappee__base( LockMode lockMode) throws DatabaseException { CursorImpl origCursor=null; CursorImpl dup=null; try { origCursor=cursorImpl; dup=origCursor.cloneCursor(true); return dup.count(getLockType(lockMode,false)); } finally { if (dup != origCursor) { dup.close(); } } }

	 int countInternal( LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	countInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 OperationStatus deleteInternal__wrappee__base() throws DatabaseException { DatabaseEntry oldKey=null; DatabaseEntry oldData=null; boolean doNotifyTriggers=dbHandle != null && dbHandle.hasTriggers(); if (doNotifyTriggers) { oldKey=new DatabaseEntry(); oldData=new DatabaseEntry(); OperationStatus status=getCurrentInternal(oldKey,oldData,LockMode.RMW); if (status != OperationStatus.SUCCESS) { return OperationStatus.KEYEMPTY; } } if (doNotifyTriggers) { dbHandle.notifyTriggers(cursorImpl.getLocker(),oldKey,oldData,null); } OperationStatus status=deleteNoNotify(); return status; }

	 OperationStatus deleteInternal() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	deleteInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 OperationStatus deleteNoNotify__wrappee__base() throws DatabaseException { CursorImpl origCursor=null; CursorImpl dup=null; OperationStatus status=OperationStatus.KEYEMPTY; try { origCursor=cursorImpl; dup=origCursor.cloneCursor(true); this.hook19(dup); status=dup.delete(); return status; } finally { this.hook20(origCursor,dup); boolean success=(status == OperationStatus.SUCCESS); if (cursorImpl == dup) { if (!success) { cursorImpl.reset(); } } else { if (success) { origCursor.close(); cursorImpl=dup; } else { dup.close(); } } } }

	 OperationStatus deleteNoNotify() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	deleteNoNotify__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 OperationStatus putInternal__wrappee__base( DatabaseEntry key, DatabaseEntry data, PutMode putMode) throws DatabaseException { DatabaseEntry oldData=null; boolean doNotifyTriggers=dbHandle != null && dbHandle.hasTriggers(); if (doNotifyTriggers && (putMode == PutMode.CURRENT || putMode == PutMode.OVERWRITE)) { oldData=new DatabaseEntry(); if (key == null && putMode == PutMode.CURRENT) { key=new DatabaseEntry(); } } OperationStatus commitStatus=putNoNotify(key,data,putMode,oldData); if (doNotifyTriggers && commitStatus == OperationStatus.SUCCESS) { if (oldData != null && oldData.getData() == null) { oldData=null; } dbHandle.notifyTriggers(cursorImpl.getLocker(),key,oldData,data); } return commitStatus; }

	 OperationStatus putInternal( DatabaseEntry key, DatabaseEntry data, PutMode putMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	putInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 OperationStatus putNoNotify__wrappee__base( DatabaseEntry key, DatabaseEntry data, PutMode putMode, DatabaseEntry returnOldData) throws DatabaseException { Locker nextKeyLocker=null; CursorImpl nextKeyCursor=null; try { Locker cursorLocker=cursorImpl.getLocker(); if (putMode != PutMode.CURRENT && dbImpl.getDbEnvironment().getTxnManager().areOtherSerializableTransactionsActive(cursorLocker)) { nextKeyLocker=new BuddyLocker(dbImpl.getDbEnvironment(),cursorLocker); nextKeyCursor=new CursorImpl(dbImpl,nextKeyLocker); nextKeyCursor.lockNextKeyForInsert(key,data); } return putAllowPhantoms(key,data,putMode,returnOldData,nextKeyCursor); } finally { if (nextKeyCursor != null) { nextKeyCursor.close(); } if (nextKeyLocker != null) { nextKeyLocker.operationEnd(); } } }

	 OperationStatus putNoNotify( DatabaseEntry key, DatabaseEntry data, PutMode putMode, DatabaseEntry returnOldData) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	putNoNotify__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private OperationStatus putAllowPhantoms__wrappee__base( DatabaseEntry key, DatabaseEntry data, PutMode putMode, DatabaseEntry returnOldData, CursorImpl nextKeyCursor) throws DatabaseException { if (data == null) { throw new NullPointerException("put passed a null DatabaseEntry arg"); } if (putMode != PutMode.CURRENT && key == null) { throw new IllegalArgumentException("put passed a null DatabaseEntry arg"); } CursorImpl origCursor=null; OperationStatus status=OperationStatus.NOTFOUND; CursorImpl dup=null; try { origCursor=cursorImpl; if (putMode == PutMode.CURRENT) { dup=origCursor.cloneCursor(true); } else { dup=origCursor.cloneCursor(false,nextKeyCursor); } if (putMode == PutMode.CURRENT) { status=dup.putCurrent(data,key,returnOldData); } else if (putMode == PutMode.OVERWRITE) { status=dup.put(key,data,returnOldData); } else if (putMode == PutMode.NOOVERWRITE) { status=dup.putNoOverwrite(key,data); } else if (putMode == PutMode.NODUP) { status=dup.putNoDupData(key,data); } else { throw new InternalException("unknown PutMode"); } return status; } finally { this.hook21(origCursor); boolean success=(status == OperationStatus.SUCCESS); if (cursorImpl == dup) { if (!success) { cursorImpl.reset(); } } else { if (success) { origCursor.close(); cursorImpl=dup; } else { if (dup != null) { dup.close(); } } } } }

	 private OperationStatus putAllowPhantoms( DatabaseEntry key, DatabaseEntry data, PutMode putMode, DatabaseEntry returnOldData, CursorImpl nextKeyCursor) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	putAllowPhantoms__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 OperationStatus position__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode, boolean first) throws DatabaseException { if (!isSerializableIsolation(lockMode)) { return positionAllowPhantoms(key,data,getLockType(lockMode,false),first); } while (true) { try { if (!first) { cursorImpl.lockEofNode(LockType.RANGE_READ); } LockType lockType=getLockType(lockMode,first); OperationStatus status=positionAllowPhantoms(key,data,lockType,first); if (first && status != OperationStatus.SUCCESS) { cursorImpl.lockEofNode(LockType.RANGE_READ); } return status; } catch ( RangeRestartException e) { continue; } } }

	 OperationStatus position( DatabaseEntry key, DatabaseEntry data, LockMode lockMode, boolean first) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	position__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private OperationStatus positionAllowPhantoms__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockType lockType, boolean first) throws DatabaseException { assert (key != null && data != null); OperationStatus status=OperationStatus.NOTFOUND; CursorImpl dup=null; try { dup=beginRead(false); if (!dup.positionFirstOrLast(first,null)) { status=OperationStatus.NOTFOUND; this.hook22(); } else { this.hook23(); status=dup.getCurrentAlreadyLatched(key,data,lockType,first); if (status == OperationStatus.SUCCESS) { if (dup.getDupBIN() != null) { dup.incrementLNCount(); } } else { status=dup.getNext(key,data,lockType,first,false); } } } finally { this.hook24(); endRead(dup,status == OperationStatus.SUCCESS); } return status; }

	 private OperationStatus positionAllowPhantoms( DatabaseEntry key, DatabaseEntry data, LockType lockType, boolean first) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	positionAllowPhantoms__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 OperationStatus search__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode, SearchMode searchMode) throws DatabaseException { if (!isSerializableIsolation(lockMode)) { LockType lockType=getLockType(lockMode,false); KeyChangeStatus result=searchAllowPhantoms(key,data,lockType,lockType,searchMode); return result.status; } while (true) { try { LockType searchLockType=getLockType(lockMode,false); LockType advanceLockType=getLockType(lockMode,true); DatabaseEntry tryKey=new DatabaseEntry(key.getData(),key.getOffset(),key.getSize()); DatabaseEntry tryData=new DatabaseEntry(data.getData(),data.getOffset(),data.getSize()); KeyChangeStatus result; if (searchMode.isExactSearch()) { result=searchExactAndRangeLock(tryKey,tryData,searchLockType,advanceLockType,searchMode); } else { result=searchAllowPhantoms(tryKey,tryData,searchLockType,advanceLockType,searchMode); if (result.status != OperationStatus.SUCCESS) { cursorImpl.lockEofNode(LockType.RANGE_READ); } } if (result.status == OperationStatus.SUCCESS) { key.setData(tryKey.getData(),0,tryKey.getSize()); data.setData(tryData.getData(),0,tryData.getSize()); } return result.status; } catch ( RangeRestartException e) { continue; } } }

	 OperationStatus search( DatabaseEntry key, DatabaseEntry data, LockMode lockMode, SearchMode searchMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	search__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private KeyChangeStatus searchExactAndRangeLock__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockType searchLockType, LockType advanceLockType, SearchMode searchMode) throws DatabaseException { searchMode=(searchMode == SearchMode.SET) ? SearchMode.SET_RANGE : SearchMode.BOTH_RANGE; KeyChangeStatus result=null; boolean noNextKeyFound; CursorImpl dup=beginRead(false); try { result=searchInternal(dup,key,data,searchLockType,advanceLockType,searchMode,true); noNextKeyFound=!result.keyChange; if (result.keyChange && result.status == OperationStatus.SUCCESS) { result.status=OperationStatus.NOTFOUND; } } finally { endRead(dup,result != null && result.status == OperationStatus.SUCCESS); } if (noNextKeyFound) { cursorImpl.lockEofNode(LockType.RANGE_READ); } return result; }

	 private KeyChangeStatus searchExactAndRangeLock( DatabaseEntry key, DatabaseEntry data, LockType searchLockType, LockType advanceLockType, SearchMode searchMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	searchExactAndRangeLock__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private KeyChangeStatus searchAllowPhantoms__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockType searchLockType, LockType advanceLockType, SearchMode searchMode) throws DatabaseException { OperationStatus status=OperationStatus.NOTFOUND; CursorImpl dup=beginRead(false); try { KeyChangeStatus result=searchInternal(dup,key,data,searchLockType,advanceLockType,searchMode,false); status=result.status; return result; } finally { endRead(dup,status == OperationStatus.SUCCESS); } }

	 private KeyChangeStatus searchAllowPhantoms( DatabaseEntry key, DatabaseEntry data, LockType searchLockType, LockType advanceLockType, SearchMode searchMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	searchAllowPhantoms__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private KeyChangeStatus searchInternal__wrappee__base( CursorImpl dup, DatabaseEntry key, DatabaseEntry data, LockType searchLockType, LockType advanceLockType, SearchMode searchMode, boolean advanceAfterRangeSearch) throws DatabaseException { assert key != null && data != null; OperationStatus status=OperationStatus.NOTFOUND; boolean keyChange=false; this.hook25(dup,key,data,searchLockType,advanceLockType,searchMode,advanceAfterRangeSearch,status,keyChange); return new KeyChangeStatus(status,keyChange); }

	 private KeyChangeStatus searchInternal( CursorImpl dup, DatabaseEntry key, DatabaseEntry data, LockType searchLockType, LockType advanceLockType, SearchMode searchMode, boolean advanceAfterRangeSearch) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	searchInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 OperationStatus retrieveNext__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode, GetMode getMode) throws DatabaseException { if (!isSerializableIsolation(lockMode)) { return retrieveNextAllowPhantoms(key,data,getLockType(lockMode,false),getMode); } while (true) { try { OperationStatus status; if (getMode == GetMode.NEXT_DUP) { status=getNextDupAndRangeLock(key,data,lockMode); } else { if (!getMode.isForward()) { rangeLockCurrentPosition(getMode); } LockType lockType=getLockType(lockMode,getMode.isForward()); status=retrieveNextAllowPhantoms(key,data,lockType,getMode); if (getMode.isForward() && status != OperationStatus.SUCCESS) { cursorImpl.lockEofNode(LockType.RANGE_READ); } } return status; } catch ( RangeRestartException e) { continue; } } }

	 OperationStatus retrieveNext( DatabaseEntry key, DatabaseEntry data, LockMode lockMode, GetMode getMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	retrieveNext__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private OperationStatus getNextDupAndRangeLock__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { DatabaseEntry tryKey=new DatabaseEntry(); DatabaseEntry tryData=new DatabaseEntry(); LockType lockType=getLockType(lockMode,true); OperationStatus status; boolean noNextKeyFound; while (true) { this.hook26(); CursorImpl dup=beginRead(true); try { KeyChangeStatus result=dup.getNextWithKeyChangeStatus(tryKey,tryData,lockType,true,false); status=result.status; noNextKeyFound=(status != OperationStatus.SUCCESS); if (result.keyChange && status == OperationStatus.SUCCESS) { status=OperationStatus.NOTFOUND; } } catch ( DatabaseException DBE) { endRead(dup,false); throw DBE; } if (checkForInsertion(GetMode.NEXT,cursorImpl,dup)) { endRead(dup,false); continue; } else { endRead(dup,status == OperationStatus.SUCCESS); this.hook27(); break; } } if (noNextKeyFound) { cursorImpl.lockEofNode(LockType.RANGE_READ); } if (status == OperationStatus.SUCCESS) { key.setData(tryKey.getData(),0,tryKey.getSize()); data.setData(tryData.getData(),0,tryData.getSize()); } return status; }

	 private OperationStatus getNextDupAndRangeLock( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getNextDupAndRangeLock__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void rangeLockCurrentPosition__wrappee__base( GetMode getMode) throws DatabaseException { DatabaseEntry tempKey=new DatabaseEntry(); DatabaseEntry tempData=new DatabaseEntry(); tempKey.setPartial(0,0,true); tempData.setPartial(0,0,true); OperationStatus status; CursorImpl dup=cursorImpl.cloneCursor(true); try { if (getMode == GetMode.PREV_NODUP) { status=dup.getFirstDuplicate(tempKey,tempData,LockType.RANGE_READ); } else { status=dup.getCurrent(tempKey,tempData,LockType.RANGE_READ); } if (status != OperationStatus.SUCCESS) { while (true) { this.hook28(); status=dup.getNext(tempKey,tempData,LockType.RANGE_READ,true,false); if (checkForInsertion(GetMode.NEXT,cursorImpl,dup)) { dup.close(); dup=cursorImpl.cloneCursor(true); continue; } else { this.hook29(); break; } } } } finally { if (cursorImpl == dup) { dup.reset(); } else { dup.close(); } } if (status != OperationStatus.SUCCESS) { cursorImpl.lockEofNode(LockType.RANGE_READ); } }

	 private void rangeLockCurrentPosition( GetMode getMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	rangeLockCurrentPosition__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private OperationStatus retrieveNextAllowPhantoms__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockType lockType, GetMode getMode) throws DatabaseException { assert (key != null && data != null); OperationStatus status; while (true) { this.hook30(); CursorImpl dup=beginRead(true); try { if (getMode == GetMode.NEXT) { status=dup.getNext(key,data,lockType,true,false); } else if (getMode == GetMode.PREV) { status=dup.getNext(key,data,lockType,false,false); } else if (getMode == GetMode.NEXT_DUP) { status=dup.getNextDuplicate(key,data,lockType,true,false); } else if (getMode == GetMode.PREV_DUP) { status=dup.getNextDuplicate(key,data,lockType,false,false); } else if (getMode == GetMode.NEXT_NODUP) { status=dup.getNextNoDup(key,data,lockType,true,false); } else if (getMode == GetMode.PREV_NODUP) { status=dup.getNextNoDup(key,data,lockType,false,false); } else { throw new InternalException("unknown GetMode"); } } catch ( DatabaseException DBE) { endRead(dup,false); throw DBE; } if (checkForInsertion(getMode,cursorImpl,dup)) { endRead(dup,false); continue; } else { endRead(dup,status == OperationStatus.SUCCESS); this.hook31(); break; } } return status; }

	 private OperationStatus retrieveNextAllowPhantoms( DatabaseEntry key, DatabaseEntry data, LockType lockType, GetMode getMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	retrieveNextAllowPhantoms__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 OperationStatus getCurrentInternal__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { LockType lockType=getLockType(lockMode,false); return cursorImpl.getCurrent(key,data,lockType); }

	 OperationStatus getCurrentInternal( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getCurrentInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private boolean checkForInsertion__wrappee__base( GetMode getMode, CursorImpl origCursor, CursorImpl dupCursor) throws DatabaseException { BIN origBIN=origCursor.getBIN(); BIN dupBIN=dupCursor.getBIN(); DBIN origDBIN=origCursor.getDupBIN(); boolean forward=true; if (getMode == GetMode.PREV || getMode == GetMode.PREV_DUP || getMode == GetMode.PREV_NODUP) { forward=false; } boolean ret=false; if (origBIN != dupBIN) { this.hook33(origCursor); if (origDBIN == null) { if (forward) { if (origBIN.getNEntries() - 1 > origCursor.getIndex()) { for (int i=origCursor.getIndex() + 1; i < origBIN.getNEntries(); i++) { if (!origBIN.isEntryKnownDeleted(i)) { Node n=origBIN.fetchTarget(i); if (n != null && !n.containsDuplicates()) { LN ln=(LN)n; if (!ln.isDeleted()) { ret=true; break; } } } else { } } } } else { if (origCursor.getIndex() > 0) { for (int i=0; i < origCursor.getIndex(); i++) { if (!origBIN.isEntryKnownDeleted(i)) { Node n=origBIN.fetchTarget(i); if (n != null && !n.containsDuplicates()) { LN ln=(LN)n; if (!ln.isDeleted()) { ret=true; break; } } else { } } } } } } this.hook32(origCursor); return ret; } if (origDBIN != dupCursor.getDupBIN() && origCursor.getIndex() == dupCursor.getIndex() && getMode != GetMode.NEXT_NODUP && getMode != GetMode.PREV_NODUP) { this.hook35(origCursor); if (forward) { if (origDBIN.getNEntries() - 1 > origCursor.getDupIndex()) { for (int i=origCursor.getDupIndex() + 1; i < origDBIN.getNEntries(); i++) { if (!origDBIN.isEntryKnownDeleted(i)) { Node n=origDBIN.fetchTarget(i); LN ln=(LN)n; if (n != null && !ln.isDeleted()) { ret=true; break; } } } } } else { if (origCursor.getDupIndex() > 0) { for (int i=0; i < origCursor.getDupIndex(); i++) { if (!origDBIN.isEntryKnownDeleted(i)) { Node n=origDBIN.fetchTarget(i); LN ln=(LN)n; if (n != null && !ln.isDeleted()) { ret=true; break; } } } } } this.hook34(origCursor); return ret; } return false; }

	 private boolean checkForInsertion( GetMode getMode, CursorImpl origCursor, CursorImpl dupCursor) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	checkForInsertion__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private CursorImpl beginRead__wrappee__base( boolean addCursor) throws DatabaseException { CursorImpl dup; if (cursorImpl.isNotInitialized()) { dup=cursorImpl; } else { dup=cursorImpl.cloneCursor(addCursor); } return dup; }

	 private CursorImpl beginRead( boolean addCursor) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	beginRead__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void endRead__wrappee__base( CursorImpl dup, boolean success) throws DatabaseException { if (dup == cursorImpl) { if (!success) { cursorImpl.reset(); } } else { if (success) { cursorImpl.close(); cursorImpl=dup; } else { dup.close(); } } }

	 private void endRead( CursorImpl dup, boolean success) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	endRead__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean advanceCursor__wrappee__base( DatabaseEntry key, DatabaseEntry data){ return cursorImpl.advanceCursor(key,data); }

	 boolean advanceCursor( DatabaseEntry key, DatabaseEntry data){ t.in(Thread.currentThread().getStackTrace()[1].toString());	advanceCursor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private LockType getLockType__wrappee__base( LockMode lockMode, boolean rangeLock){ if (isReadUncommittedMode(lockMode)) { return LockType.NONE; } else if (lockMode == null || lockMode == LockMode.DEFAULT) { return rangeLock ? LockType.RANGE_READ : LockType.READ; } else if (lockMode == LockMode.RMW) { return rangeLock ? LockType.RANGE_WRITE : LockType.WRITE; } else if (lockMode == LockMode.READ_COMMITTED) { throw new IllegalArgumentException(lockMode.toString() + " not allowed with Cursor methods"); } else { assert false : lockMode; return LockType.NONE; } }

	 private LockType getLockType( LockMode lockMode, boolean rangeLock){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLockType__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean isReadUncommittedMode__wrappee__base( LockMode lockMode){ return (lockMode == LockMode.READ_UNCOMMITTED || (readUncommittedDefault && (lockMode == null || lockMode == LockMode.DEFAULT))); }

	 boolean isReadUncommittedMode( LockMode lockMode){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isReadUncommittedMode__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private boolean isSerializableIsolation__wrappee__base( LockMode lockMode){ return serializableIsolationDefault && !isReadUncommittedMode(lockMode); }

	 private boolean isSerializableIsolation( LockMode lockMode){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isSerializableIsolation__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void checkUpdatesAllowed__wrappee__base( String operation) throws DatabaseException { if (updateOperationsProhibited) { throw new DatabaseException("A transaction was not supplied when opening this cursor: " + operation); } }

	 protected void checkUpdatesAllowed( String operation) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	checkUpdatesAllowed__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void checkArgsNoValRequired__wrappee__base( DatabaseEntry key, DatabaseEntry data){ DatabaseUtil.checkForNullDbt(key,"key",false); DatabaseUtil.checkForNullDbt(data,"data",false); }

	 private void checkArgsNoValRequired( DatabaseEntry key, DatabaseEntry data){ t.in(Thread.currentThread().getStackTrace()[1].toString());	checkArgsNoValRequired__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void checkArgsValRequired__wrappee__base( DatabaseEntry key, DatabaseEntry data){ DatabaseUtil.checkForNullDbt(key,"key",true); DatabaseUtil.checkForNullDbt(data,"data",true); }

	 private void checkArgsValRequired( DatabaseEntry key, DatabaseEntry data){ t.in(Thread.currentThread().getStackTrace()[1].toString());	checkArgsValRequired__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void checkState__wrappee__base( boolean mustBeInitialized) throws DatabaseException { checkEnv(); cursorImpl.checkCursorState(mustBeInitialized); }

	 void checkState( boolean mustBeInitialized) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	checkState__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void checkEnv__wrappee__base() throws RunRecoveryException { cursorImpl.checkEnv(); }

	 void checkEnv() throws RunRecoveryException { t.in(Thread.currentThread().getStackTrace()[1].toString());	checkEnv__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void traceCursorImpl__wrappee__base( StringBuffer sb){ sb.append(" locker=").append(cursorImpl.getLocker().getId()); if (cursorImpl.getBIN() != null) { sb.append(" bin=").append(cursorImpl.getBIN().getNodeId()); } sb.append(" idx=").append(cursorImpl.getIndex()); if (cursorImpl.getDupBIN() != null) { sb.append(" Dbin=").append(cursorImpl.getDupBIN().getNodeId()); } sb.append(" dupIdx=").append(cursorImpl.getDupIndex()); }

	 private void traceCursorImpl( StringBuffer sb){ t.in(Thread.currentThread().getStackTrace()[1].toString());	traceCursorImpl__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook0__wrappee__base() throws DatabaseException { }

	 protected void hook0() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook0__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook1__wrappee__base() throws DatabaseException { }

	 protected void hook1() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook1__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook2__wrappee__base( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { }

	 protected void hook2( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook2__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook3__wrappee__base( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { }

	 protected void hook3( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook3__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook4__wrappee__base( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { }

	 protected void hook4( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook4__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook5__wrappee__base( DatabaseEntry data) throws DatabaseException { }

	 protected void hook5( DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook5__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook6__wrappee__base( LockMode lockMode) throws DatabaseException { }

	 protected void hook6( LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook6__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook7__wrappee__base( LockMode lockMode) throws DatabaseException { }

	 protected void hook7( LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook7__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook8__wrappee__base( LockMode lockMode) throws DatabaseException { }

	 protected void hook8( LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook8__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook9__wrappee__base( LockMode lockMode) throws DatabaseException { }

	 protected void hook9( LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook9__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook10__wrappee__base( LockMode lockMode) throws DatabaseException { }

	 protected void hook10( LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook10__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook11__wrappee__base( LockMode lockMode) throws DatabaseException { }

	 protected void hook11( LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook11__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook12__wrappee__base( LockMode lockMode) throws DatabaseException { }

	 protected void hook12( LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook12__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook13__wrappee__base( LockMode lockMode) throws DatabaseException { }

	 protected void hook13( LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook13__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook14__wrappee__base( LockMode lockMode) throws DatabaseException { }

	 protected void hook14( LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook14__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook15__wrappee__base( DatabaseEntry key, LockMode lockMode) throws DatabaseException { }

	 protected void hook15( DatabaseEntry key, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook15__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook16__wrappee__base( DatabaseEntry key, LockMode lockMode) throws DatabaseException { }

	 protected void hook16( DatabaseEntry key, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook16__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook17__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { }

	 protected void hook17( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook17__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook18__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { }

	 protected void hook18( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook18__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook19__wrappee__base( CursorImpl dup) throws DatabaseException { }

	 protected void hook19( CursorImpl dup) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook19__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook20__wrappee__base( CursorImpl origCursor, CursorImpl dup) throws DatabaseException { }

	 protected void hook20( CursorImpl origCursor, CursorImpl dup) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook20__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook21__wrappee__base( CursorImpl origCursor) throws DatabaseException { }

	 protected void hook21( CursorImpl origCursor) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook21__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook22__wrappee__base() throws DatabaseException { }

	 protected void hook22() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook22__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook23__wrappee__base() throws DatabaseException { }

	 protected void hook23() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook23__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook24__wrappee__base() throws DatabaseException { }

	 protected void hook24() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook24__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook25__wrappee__base( CursorImpl dup, DatabaseEntry key, DatabaseEntry data, LockType searchLockType, LockType advanceLockType, SearchMode searchMode, boolean advanceAfterRangeSearch, OperationStatus status, boolean keyChange) throws DatabaseException { int searchResult=dup.searchAndPosition(key,data,searchMode,searchLockType); if ((searchResult & CursorImpl.FOUND) != 0) { boolean exactKeyMatch=((searchResult & CursorImpl.EXACT_KEY) != 0); boolean exactDataMatch=((searchResult & CursorImpl.EXACT_DATA) != 0); boolean foundLast=((searchResult & CursorImpl.FOUND_LAST) != 0); boolean rangeMatch=false; if (searchMode == SearchMode.SET_RANGE && !exactKeyMatch) { rangeMatch=true; } if (searchMode == SearchMode.BOTH_RANGE && (!exactKeyMatch || !exactDataMatch)) { rangeMatch=true; } DatabaseEntry useKey=(searchMode == SearchMode.SET) ? null : key; if (rangeMatch || (status=dup.getCurrentAlreadyLatched(useKey,data,searchLockType,true)) == OperationStatus.KEYEMPTY) { if (foundLast) { status=OperationStatus.NOTFOUND; } else if (searchMode == SearchMode.SET) { status=dup.getNextDuplicate(key,data,advanceLockType,true,rangeMatch); } else if (searchMode == SearchMode.BOTH) { if (status == OperationStatus.KEYEMPTY) { status=OperationStatus.NOTFOUND; } } else { assert !searchMode.isExactSearch(); byte[] searchKey=null; if (searchMode.isDataSearch()) { searchKey=Key.makeKey(key); } if (exactKeyMatch) { KeyChangeStatus result=dup.getNextWithKeyChangeStatus(key,data,advanceLockType,true,rangeMatch); status=result.status; keyChange=searchMode.isDataSearch() ? (status == OperationStatus.SUCCESS) : result.keyChange; } else if (searchMode.isDataSearch() && !advanceAfterRangeSearch) { status=OperationStatus.NOTFOUND; } else { status=dup.getNextNoDup(key,data,advanceLockType,true,rangeMatch); keyChange=(status == OperationStatus.SUCCESS); } if (status == OperationStatus.SUCCESS && searchMode.isDataSearch()) { if (Key.compareKeys(key.getData(),searchKey,dbImpl.getDuplicateComparator()) != 0) { status=OperationStatus.NOTFOUND; } } } } } }

	 protected void hook25( CursorImpl dup, DatabaseEntry key, DatabaseEntry data, LockType searchLockType, LockType advanceLockType, SearchMode searchMode, boolean advanceAfterRangeSearch, OperationStatus status, boolean keyChange) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook25__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook26__wrappee__base() throws DatabaseException { }

	 protected void hook26() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook26__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook27__wrappee__base() throws DatabaseException { }

	 protected void hook27() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook27__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook28__wrappee__base() throws DatabaseException { }

	 protected void hook28() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook28__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook29__wrappee__base() throws DatabaseException { }

	 protected void hook29() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook29__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook30__wrappee__base() throws DatabaseException { }

	 protected void hook30() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook30__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook31__wrappee__base() throws DatabaseException { }

	 protected void hook31() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook31__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook32__wrappee__base( CursorImpl origCursor) throws DatabaseException { }

	 protected void hook32( CursorImpl origCursor) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook32__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook33__wrappee__base( CursorImpl origCursor) throws DatabaseException { }

	 protected void hook33( CursorImpl origCursor) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook33__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook34__wrappee__base( CursorImpl origCursor) throws DatabaseException { }

	 protected void hook34( CursorImpl origCursor) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook34__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook35__wrappee__base( CursorImpl origCursor) throws DatabaseException { }

	 protected void hook35( CursorImpl origCursor) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook35__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook36__wrappee__base( DatabaseImpl dbImpl) throws DatabaseException { }

	 protected void hook36( DatabaseImpl dbImpl) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook36__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
