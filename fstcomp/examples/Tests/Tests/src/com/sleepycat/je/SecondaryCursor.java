package com.sleepycat.je; 
import java.util.HashSet; 
import java.util.Set; 
import java.util.logging.Level; 
import com.sleepycat.je.dbi.GetMode; 
import com.sleepycat.je.dbi.CursorImpl.SearchMode; 
import com.sleepycat.je.txn.Locker; 
import de.ovgu.cide.jakutil.*; 
public  class  SecondaryCursor  extends Cursor {
	 private SecondaryDatabase secondaryDb;

	 private Database primaryDb;

	 SecondaryCursor( SecondaryDatabase dbHandle, Transaction txn, CursorConfig cursorConfig) throws DatabaseException { super(dbHandle,txn,cursorConfig); secondaryDb=dbHandle; primaryDb=dbHandle.getPrimaryDatabase(); }

	 private SecondaryCursor( SecondaryCursor cursor, boolean samePosition) throws DatabaseException { super(cursor,samePosition); secondaryDb=cursor.secondaryDb; primaryDb=cursor.primaryDb; }

	 public Database getPrimaryDatabase(){ return primaryDb; }

	 public Cursor dup( boolean samePosition) throws DatabaseException { checkState(true); return new SecondaryCursor(this,samePosition); }

	 public SecondaryCursor dupSecondary( boolean samePosition) throws DatabaseException { return (SecondaryCursor)dup(samePosition); }

	 public OperationStatus delete() throws DatabaseException { checkState(true); checkUpdatesAllowed("delete"); this.hook65(); DatabaseEntry key=new DatabaseEntry(); DatabaseEntry pKey=new DatabaseEntry(); OperationStatus status=getCurrentInternal(key,pKey,LockMode.RMW); if (status == OperationStatus.SUCCESS) { status=primaryDb.deleteInternal(cursorImpl.getLocker(),pKey); } return status; }

	 public OperationStatus put( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { throw SecondaryDatabase.notAllowedException(); }

	 public OperationStatus putNoOverwrite( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { throw SecondaryDatabase.notAllowedException(); }

	 public OperationStatus putNoDupData( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { throw SecondaryDatabase.notAllowedException(); }

	 public OperationStatus putCurrent( DatabaseEntry data) throws DatabaseException { throw SecondaryDatabase.notAllowedException(); }

	 public OperationStatus getCurrent( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { return getCurrent(key,new DatabaseEntry(),data,lockMode); }

	 public OperationStatus getCurrent( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(true); checkArgsNoValRequired(key,pKey,data); this.hook66(lockMode); return getCurrentInternal(key,pKey,data,lockMode); }

	 public OperationStatus getFirst( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { return getFirst(key,new DatabaseEntry(),data,lockMode); }

	 public OperationStatus getFirst( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); checkArgsNoValRequired(key,pKey,data); this.hook67(lockMode); return position(key,pKey,data,lockMode,true); }

	 public OperationStatus getLast( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { return getLast(key,new DatabaseEntry(),data,lockMode); }

	 public OperationStatus getLast( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); checkArgsNoValRequired(key,pKey,data); this.hook68(lockMode); return position(key,pKey,data,lockMode,false); }

	 public OperationStatus getNext( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { return getNext(key,new DatabaseEntry(),data,lockMode); }

	 public OperationStatus getNext( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); checkArgsNoValRequired(key,pKey,data); this.hook69(lockMode); if (cursorImpl.isNotInitialized()) { return position(key,pKey,data,lockMode,true); } else { return retrieveNext(key,pKey,data,lockMode,GetMode.NEXT); } }

	 public OperationStatus getNextDup( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { return getNextDup(key,new DatabaseEntry(),data,lockMode); }

	 public OperationStatus getNextDup( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(true); checkArgsNoValRequired(key,pKey,data); this.hook70(lockMode); return retrieveNext(key,pKey,data,lockMode,GetMode.NEXT_DUP); }

	 public OperationStatus getNextNoDup( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { return getNextNoDup(key,new DatabaseEntry(),data,lockMode); }

	 public OperationStatus getNextNoDup( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); checkArgsNoValRequired(key,pKey,data); this.hook71(lockMode); if (cursorImpl.isNotInitialized()) { return position(key,pKey,data,lockMode,true); } else { return retrieveNext(key,pKey,data,lockMode,GetMode.NEXT_NODUP); } }

	 public OperationStatus getPrev( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { return getPrev(key,new DatabaseEntry(),data,lockMode); }

	 public OperationStatus getPrev( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); checkArgsNoValRequired(key,pKey,data); this.hook72(lockMode); if (cursorImpl.isNotInitialized()) { return position(key,pKey,data,lockMode,false); } else { return retrieveNext(key,pKey,data,lockMode,GetMode.PREV); } }

	 public OperationStatus getPrevDup( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { return getPrevDup(key,new DatabaseEntry(),data,lockMode); }

	 public OperationStatus getPrevDup( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(true); checkArgsNoValRequired(key,pKey,data); this.hook73(lockMode); return retrieveNext(key,pKey,data,lockMode,GetMode.PREV_DUP); }

	 public OperationStatus getPrevNoDup( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { return getPrevNoDup(key,new DatabaseEntry(),data,lockMode); }

	 public OperationStatus getPrevNoDup( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); checkArgsNoValRequired(key,pKey,data); this.hook74(lockMode); if (cursorImpl.isNotInitialized()) { return position(key,pKey,data,lockMode,false); } else { return retrieveNext(key,pKey,data,lockMode,GetMode.PREV_NODUP); } }

	 public OperationStatus getSearchKey( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { return getSearchKey(key,new DatabaseEntry(),data,lockMode); }

	 public OperationStatus getSearchKey( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); DatabaseUtil.checkForNullDbt(key,"key",true); DatabaseUtil.checkForNullDbt(pKey,"pKey",false); DatabaseUtil.checkForNullDbt(data,"data",false); this.hook75(key,lockMode); return search(key,pKey,data,lockMode,SearchMode.SET); }

	 public OperationStatus getSearchKeyRange( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { return getSearchKeyRange(key,new DatabaseEntry(),data,lockMode); }

	 public OperationStatus getSearchKeyRange( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); DatabaseUtil.checkForNullDbt(key,"key",true); DatabaseUtil.checkForNullDbt(pKey,"pKey",false); DatabaseUtil.checkForNullDbt(data,"data",false); this.hook76(key,data,lockMode); return search(key,pKey,data,lockMode,SearchMode.SET_RANGE); }

	 public OperationStatus getSearchBoth( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { throw SecondaryDatabase.notAllowedException(); }

	 public OperationStatus getSearchBoth( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); DatabaseUtil.checkForNullDbt(key,"key",true); DatabaseUtil.checkForNullDbt(pKey,"pKey",true); DatabaseUtil.checkForNullDbt(data,"data",false); this.hook77(key,data,lockMode); return search(key,pKey,data,lockMode,SearchMode.BOTH); }

	 public OperationStatus getSearchBothRange( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { throw SecondaryDatabase.notAllowedException(); }

	 public OperationStatus getSearchBothRange( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); DatabaseUtil.checkForNullDbt(key,"key",true); DatabaseUtil.checkForNullDbt(pKey,"pKey",true); DatabaseUtil.checkForNullDbt(data,"data",false); this.hook78(key,data,lockMode); return search(key,pKey,data,lockMode,SearchMode.BOTH_RANGE); }

	 private OperationStatus getCurrentInternal( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { OperationStatus status=getCurrentInternal(key,pKey,lockMode); if (status == OperationStatus.SUCCESS) { status=readPrimaryAfterGet(key,pKey,data,lockMode); } return status; }

	 OperationStatus search( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode, SearchMode searchMode) throws DatabaseException { while (true) { OperationStatus status=search(key,pKey,lockMode,searchMode); if (status != OperationStatus.SUCCESS) { return status; } status=readPrimaryAfterGet(key,pKey,data,lockMode); if (status == OperationStatus.SUCCESS) { return status; } } }

	 OperationStatus position( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode, boolean first) throws DatabaseException { while (true) { OperationStatus status=position(key,pKey,lockMode,first); if (status != OperationStatus.SUCCESS) { return status; } status=readPrimaryAfterGet(key,pKey,data,lockMode); if (status == OperationStatus.SUCCESS) { return status; } } }

	 OperationStatus retrieveNext( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode, GetMode getMode) throws DatabaseException { while (true) { OperationStatus status=retrieveNext(key,pKey,lockMode,getMode); if (status != OperationStatus.SUCCESS) { return status; } status=readPrimaryAfterGet(key,pKey,data,lockMode); if (status == OperationStatus.SUCCESS) { return status; } } }

	 private OperationStatus readPrimaryAfterGet( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { Locker locker=cursorImpl.getLocker(); Cursor cursor=null; try { cursor=new Cursor(primaryDb,locker,null); OperationStatus status=cursor.search(pKey,data,lockMode,SearchMode.SET); if (status != OperationStatus.SUCCESS) { if (isReadUncommittedMode(lockMode)) { status=getCurrentInternal(key,pKey,lockMode); if (status == OperationStatus.KEYEMPTY) { return status; } } SecondaryDatabase secDb=(SecondaryDatabase)getDatabase(); throw secDb.secondaryCorruptException(); } if (isReadUncommittedMode(lockMode)) { SecondaryConfig config=secondaryDb.getPrivateSecondaryConfig(); if (config.getImmutableSecondaryKey()) { } else if (config.getKeyCreator() != null) { DatabaseEntry secKey=new DatabaseEntry(); if (!config.getKeyCreator().createSecondaryKey(secondaryDb,pKey,data,secKey) || !secKey.equals(key)) { return OperationStatus.KEYEMPTY; } } else if (config.getMultiKeyCreator() != null) { Set results=new HashSet(); config.getMultiKeyCreator().createSecondaryKeys(secondaryDb,pKey,data,results); if (!results.contains(key)) { return OperationStatus.KEYEMPTY; } } } return OperationStatus.SUCCESS; } finally { if (cursor != null) { cursor.close(); } } }

	 private void checkArgsNoValRequired( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data){ DatabaseUtil.checkForNullDbt(key,"key",false); DatabaseUtil.checkForNullDbt(pKey,"pKey",false); DatabaseUtil.checkForNullDbt(data,"data",false); }

	 protected void hook65() throws DatabaseException { }

	 protected void hook66( LockMode lockMode) throws DatabaseException { }

	 protected void hook67( LockMode lockMode) throws DatabaseException { }

	 protected void hook68( LockMode lockMode) throws DatabaseException { }

	 protected void hook69( LockMode lockMode) throws DatabaseException { }

	 protected void hook70( LockMode lockMode) throws DatabaseException { }

	 protected void hook71( LockMode lockMode) throws DatabaseException { }

	 protected void hook72( LockMode lockMode) throws DatabaseException { }

	 protected void hook73( LockMode lockMode) throws DatabaseException { }

	 protected void hook74( LockMode lockMode) throws DatabaseException { }

	 protected void hook75( DatabaseEntry key, LockMode lockMode) throws DatabaseException { }

	 protected void hook76( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { }

	 protected void hook77( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { }

	 protected void hook78( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { }


}
