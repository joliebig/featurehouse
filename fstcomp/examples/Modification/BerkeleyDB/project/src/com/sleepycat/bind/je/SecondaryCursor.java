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

	 public Database getPrimaryDatabase__wrappee__base(){ return primaryDb; }

	 public Database getPrimaryDatabase(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getPrimaryDatabase__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Cursor dup__wrappee__base( boolean samePosition) throws DatabaseException { checkState(true); return new SecondaryCursor(this,samePosition); }

	 public Cursor dup( boolean samePosition) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	dup__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public SecondaryCursor dupSecondary__wrappee__base( boolean samePosition) throws DatabaseException { return (SecondaryCursor)dup(samePosition); }

	 public SecondaryCursor dupSecondary( boolean samePosition) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	dupSecondary__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus delete__wrappee__base() throws DatabaseException { checkState(true); checkUpdatesAllowed("delete"); this.hook65(); DatabaseEntry key=new DatabaseEntry(); DatabaseEntry pKey=new DatabaseEntry(); OperationStatus status=getCurrentInternal(key,pKey,LockMode.RMW); if (status == OperationStatus.SUCCESS) { status=primaryDb.deleteInternal(cursorImpl.getLocker(),pKey); } return status; }

	 public OperationStatus delete() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	delete__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus put__wrappee__base( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { throw SecondaryDatabase.notAllowedException(); }

	 public OperationStatus put( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	put__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus putNoOverwrite__wrappee__base( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { throw SecondaryDatabase.notAllowedException(); }

	 public OperationStatus putNoOverwrite( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	putNoOverwrite__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus putNoDupData__wrappee__base( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { throw SecondaryDatabase.notAllowedException(); }

	 public OperationStatus putNoDupData( DatabaseEntry key, DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	putNoDupData__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus putCurrent__wrappee__base( DatabaseEntry data) throws DatabaseException { throw SecondaryDatabase.notAllowedException(); }

	 public OperationStatus putCurrent( DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	putCurrent__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getCurrent__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { return getCurrent(key,new DatabaseEntry(),data,lockMode); }

	 public OperationStatus getCurrent( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getCurrent__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getCurrent__wrappee__base( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(true); checkArgsNoValRequired(key,pKey,data); this.hook66(lockMode); return getCurrentInternal(key,pKey,data,lockMode); }

	 public OperationStatus getCurrent( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getCurrent__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getFirst__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { return getFirst(key,new DatabaseEntry(),data,lockMode); }

	 public OperationStatus getFirst( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getFirst__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getFirst__wrappee__base( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); checkArgsNoValRequired(key,pKey,data); this.hook67(lockMode); return position(key,pKey,data,lockMode,true); }

	 public OperationStatus getFirst( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getFirst__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getLast__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { return getLast(key,new DatabaseEntry(),data,lockMode); }

	 public OperationStatus getLast( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getLast__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getLast__wrappee__base( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); checkArgsNoValRequired(key,pKey,data); this.hook68(lockMode); return position(key,pKey,data,lockMode,false); }

	 public OperationStatus getLast( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getLast__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getNext__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { return getNext(key,new DatabaseEntry(),data,lockMode); }

	 public OperationStatus getNext( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getNext__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getNext__wrappee__base( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); checkArgsNoValRequired(key,pKey,data); this.hook69(lockMode); if (cursorImpl.isNotInitialized()) { return position(key,pKey,data,lockMode,true); } else { return retrieveNext(key,pKey,data,lockMode,GetMode.NEXT); } }

	 public OperationStatus getNext( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getNext__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getNextDup__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { return getNextDup(key,new DatabaseEntry(),data,lockMode); }

	 public OperationStatus getNextDup( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getNextDup__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getNextDup__wrappee__base( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(true); checkArgsNoValRequired(key,pKey,data); this.hook70(lockMode); return retrieveNext(key,pKey,data,lockMode,GetMode.NEXT_DUP); }

	 public OperationStatus getNextDup( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getNextDup__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getNextNoDup__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { return getNextNoDup(key,new DatabaseEntry(),data,lockMode); }

	 public OperationStatus getNextNoDup( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getNextNoDup__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getNextNoDup__wrappee__base( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); checkArgsNoValRequired(key,pKey,data); this.hook71(lockMode); if (cursorImpl.isNotInitialized()) { return position(key,pKey,data,lockMode,true); } else { return retrieveNext(key,pKey,data,lockMode,GetMode.NEXT_NODUP); } }

	 public OperationStatus getNextNoDup( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getNextNoDup__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getPrev__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { return getPrev(key,new DatabaseEntry(),data,lockMode); }

	 public OperationStatus getPrev( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getPrev__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getPrev__wrappee__base( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); checkArgsNoValRequired(key,pKey,data); this.hook72(lockMode); if (cursorImpl.isNotInitialized()) { return position(key,pKey,data,lockMode,false); } else { return retrieveNext(key,pKey,data,lockMode,GetMode.PREV); } }

	 public OperationStatus getPrev( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getPrev__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getPrevDup__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { return getPrevDup(key,new DatabaseEntry(),data,lockMode); }

	 public OperationStatus getPrevDup( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getPrevDup__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getPrevDup__wrappee__base( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(true); checkArgsNoValRequired(key,pKey,data); this.hook73(lockMode); return retrieveNext(key,pKey,data,lockMode,GetMode.PREV_DUP); }

	 public OperationStatus getPrevDup( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getPrevDup__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getPrevNoDup__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { return getPrevNoDup(key,new DatabaseEntry(),data,lockMode); }

	 public OperationStatus getPrevNoDup( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getPrevNoDup__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getPrevNoDup__wrappee__base( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); checkArgsNoValRequired(key,pKey,data); this.hook74(lockMode); if (cursorImpl.isNotInitialized()) { return position(key,pKey,data,lockMode,false); } else { return retrieveNext(key,pKey,data,lockMode,GetMode.PREV_NODUP); } }

	 public OperationStatus getPrevNoDup( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getPrevNoDup__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getSearchKey__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { return getSearchKey(key,new DatabaseEntry(),data,lockMode); }

	 public OperationStatus getSearchKey( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getSearchKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getSearchKey__wrappee__base( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); DatabaseUtil.checkForNullDbt(key,"key",true); DatabaseUtil.checkForNullDbt(pKey,"pKey",false); DatabaseUtil.checkForNullDbt(data,"data",false); this.hook75(key,lockMode); return search(key,pKey,data,lockMode,SearchMode.SET); }

	 public OperationStatus getSearchKey( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getSearchKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getSearchKeyRange__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { return getSearchKeyRange(key,new DatabaseEntry(),data,lockMode); }

	 public OperationStatus getSearchKeyRange( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getSearchKeyRange__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getSearchKeyRange__wrappee__base( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); DatabaseUtil.checkForNullDbt(key,"key",true); DatabaseUtil.checkForNullDbt(pKey,"pKey",false); DatabaseUtil.checkForNullDbt(data,"data",false); this.hook76(key,data,lockMode); return search(key,pKey,data,lockMode,SearchMode.SET_RANGE); }

	 public OperationStatus getSearchKeyRange( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getSearchKeyRange__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getSearchBoth__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { throw SecondaryDatabase.notAllowedException(); }

	 public OperationStatus getSearchBoth( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getSearchBoth__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getSearchBoth__wrappee__base( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); DatabaseUtil.checkForNullDbt(key,"key",true); DatabaseUtil.checkForNullDbt(pKey,"pKey",true); DatabaseUtil.checkForNullDbt(data,"data",false); this.hook77(key,data,lockMode); return search(key,pKey,data,lockMode,SearchMode.BOTH); }

	 public OperationStatus getSearchBoth( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getSearchBoth__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getSearchBothRange__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { throw SecondaryDatabase.notAllowedException(); }

	 public OperationStatus getSearchBothRange( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getSearchBothRange__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getSearchBothRange__wrappee__base( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkState(false); DatabaseUtil.checkForNullDbt(key,"key",true); DatabaseUtil.checkForNullDbt(pKey,"pKey",true); DatabaseUtil.checkForNullDbt(data,"data",false); this.hook78(key,data,lockMode); return search(key,pKey,data,lockMode,SearchMode.BOTH_RANGE); }

	 public OperationStatus getSearchBothRange( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getSearchBothRange__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private OperationStatus getCurrentInternal__wrappee__base( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { OperationStatus status=getCurrentInternal(key,pKey,lockMode); if (status == OperationStatus.SUCCESS) { status=readPrimaryAfterGet(key,pKey,data,lockMode); } return status; }

	 private OperationStatus getCurrentInternal( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getCurrentInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 OperationStatus search__wrappee__base( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode, SearchMode searchMode) throws DatabaseException { while (true) { OperationStatus status=search(key,pKey,lockMode,searchMode); if (status != OperationStatus.SUCCESS) { return status; } status=readPrimaryAfterGet(key,pKey,data,lockMode); if (status == OperationStatus.SUCCESS) { return status; } } }

	 OperationStatus search( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode, SearchMode searchMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	search__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 OperationStatus position__wrappee__base( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode, boolean first) throws DatabaseException { while (true) { OperationStatus status=position(key,pKey,lockMode,first); if (status != OperationStatus.SUCCESS) { return status; } status=readPrimaryAfterGet(key,pKey,data,lockMode); if (status == OperationStatus.SUCCESS) { return status; } } }

	 OperationStatus position( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode, boolean first) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	position__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 OperationStatus retrieveNext__wrappee__base( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode, GetMode getMode) throws DatabaseException { while (true) { OperationStatus status=retrieveNext(key,pKey,lockMode,getMode); if (status != OperationStatus.SUCCESS) { return status; } status=readPrimaryAfterGet(key,pKey,data,lockMode); if (status == OperationStatus.SUCCESS) { return status; } } }

	 OperationStatus retrieveNext( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode, GetMode getMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	retrieveNext__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private OperationStatus readPrimaryAfterGet__wrappee__base( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { Locker locker=cursorImpl.getLocker(); Cursor cursor=null; try { cursor=new Cursor(primaryDb,locker,null); OperationStatus status=cursor.search(pKey,data,lockMode,SearchMode.SET); if (status != OperationStatus.SUCCESS) { if (isReadUncommittedMode(lockMode)) { status=getCurrentInternal(key,pKey,lockMode); if (status == OperationStatus.KEYEMPTY) { return status; } } SecondaryDatabase secDb=(SecondaryDatabase)getDatabase(); throw secDb.secondaryCorruptException(); } if (isReadUncommittedMode(lockMode)) { SecondaryConfig config=secondaryDb.getPrivateSecondaryConfig(); if (config.getImmutableSecondaryKey()) { } else if (config.getKeyCreator() != null) { DatabaseEntry secKey=new DatabaseEntry(); if (!config.getKeyCreator().createSecondaryKey(secondaryDb,pKey,data,secKey) || !secKey.equals(key)) { return OperationStatus.KEYEMPTY; } } else if (config.getMultiKeyCreator() != null) { Set results=new HashSet(); config.getMultiKeyCreator().createSecondaryKeys(secondaryDb,pKey,data,results); if (!results.contains(key)) { return OperationStatus.KEYEMPTY; } } } return OperationStatus.SUCCESS; } finally { if (cursor != null) { cursor.close(); } } }

	 private OperationStatus readPrimaryAfterGet( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readPrimaryAfterGet__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void checkArgsNoValRequired__wrappee__base( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data){ DatabaseUtil.checkForNullDbt(key,"key",false); DatabaseUtil.checkForNullDbt(pKey,"pKey",false); DatabaseUtil.checkForNullDbt(data,"data",false); }

	 private void checkArgsNoValRequired( DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data){ t.in(Thread.currentThread().getStackTrace()[1].toString());	checkArgsNoValRequired__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook65__wrappee__base() throws DatabaseException { }

	 protected void hook65() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook65__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook66__wrappee__base( LockMode lockMode) throws DatabaseException { }

	 protected void hook66( LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook66__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook67__wrappee__base( LockMode lockMode) throws DatabaseException { }

	 protected void hook67( LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook67__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook68__wrappee__base( LockMode lockMode) throws DatabaseException { }

	 protected void hook68( LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook68__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook69__wrappee__base( LockMode lockMode) throws DatabaseException { }

	 protected void hook69( LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook69__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook70__wrappee__base( LockMode lockMode) throws DatabaseException { }

	 protected void hook70( LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook70__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook71__wrappee__base( LockMode lockMode) throws DatabaseException { }

	 protected void hook71( LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook71__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook72__wrappee__base( LockMode lockMode) throws DatabaseException { }

	 protected void hook72( LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook72__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook73__wrappee__base( LockMode lockMode) throws DatabaseException { }

	 protected void hook73( LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook73__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook74__wrappee__base( LockMode lockMode) throws DatabaseException { }

	 protected void hook74( LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook74__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook75__wrappee__base( DatabaseEntry key, LockMode lockMode) throws DatabaseException { }

	 protected void hook75( DatabaseEntry key, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook75__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook76__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { }

	 protected void hook76( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook76__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook77__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { }

	 protected void hook77( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook77__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook78__wrappee__base( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { }

	 protected void hook78( DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook78__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
