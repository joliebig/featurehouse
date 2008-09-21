package com.sleepycat.je; 
import java.util.Collections; 
import java.util.HashSet; 
import java.util.Iterator; 
import java.util.Set; 
import java.util.logging.Level; 
import java.util.logging.Logger; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.dbi.GetMode; 
import com.sleepycat.je.dbi.PutMode; 
import com.sleepycat.je.dbi.CursorImpl.SearchMode; 
import com.sleepycat.je.txn.Locker; 
import com.sleepycat.je.txn.LockerFactory; 
import de.ovgu.cide.jakutil.*; 
public  class  SecondaryDatabase  extends Database {
	 private Database primaryDb;

	 private SecondaryConfig secondaryConfig;

	 private SecondaryTrigger secondaryTrigger;

	 private ForeignKeyTrigger foreignKeyTrigger;

	 SecondaryDatabase( Environment env, SecondaryConfig secConfig, Database primaryDatabase) throws DatabaseException { super(env); DatabaseUtil.checkForNullParam(primaryDatabase,"primaryDatabase"); primaryDatabase.checkRequiredDbState(OPEN,"Can't use as primary:"); if (primaryDatabase.configuration.getSortedDuplicates()) { throw new IllegalArgumentException("Duplicates must not be allowed for a primary database: " + primaryDatabase.getDebugName()); } if (env.getEnvironmentImpl() != primaryDatabase.getEnvironment().getEnvironmentImpl()) { throw new IllegalArgumentException("Primary and secondary databases must be in the same" + " environment"); } if (secConfig.getKeyCreator() != null && secConfig.getMultiKeyCreator() != null) { throw new IllegalArgumentException("secConfig.getKeyCreator() and getMultiKeyCreator() may not" + " both be non-null"); } if (!primaryDatabase.configuration.getReadOnly() && secConfig.getKeyCreator() == null && secConfig.getMultiKeyCreator() == null) { throw new NullPointerException("secConfig and getKeyCreator()/getMultiKeyCreator()" + " may be null only if the primary database is read-only"); } if (secConfig.getForeignKeyNullifier() != null && secConfig.getForeignMultiKeyNullifier() != null) { throw new IllegalArgumentException("secConfig.getForeignKeyNullifier() and" + " getForeignMultiKeyNullifier() may not both be non-null"); } if (secConfig.getForeignKeyDeleteAction() == ForeignKeyDeleteAction.NULLIFY && secConfig.getForeignKeyNullifier() == null && secConfig.getForeignMultiKeyNullifier() == null) { throw new NullPointerException("ForeignKeyNullifier or ForeignMultiKeyNullifier must be" + " non-null when ForeignKeyDeleteAction is NULLIFY"); } if (secConfig.getForeignKeyNullifier() != null && secConfig.getMultiKeyCreator() != null) { throw new IllegalArgumentException("ForeignKeyNullifier may not be used with" + " SecondaryMultiKeyCreator -- use" + " ForeignMultiKeyNullifier instead"); } if (secConfig.getForeignKeyDatabase() != null) { Database foreignDb=secConfig.getForeignKeyDatabase(); if (foreignDb.getDatabaseImpl().getSortedDuplicates()) { throw new IllegalArgumentException("Duplicates must not be allowed for a foreign key " + " database: " + foreignDb.getDebugName()); } } primaryDb=primaryDatabase; secondaryTrigger=new SecondaryTrigger(this); if (secConfig.getForeignKeyDatabase() != null) { foreignKeyTrigger=new ForeignKeyTrigger(this); } }

	 void initNew__wrappee__base( Environment env, Locker locker, String databaseName, DatabaseConfig dbConfig) throws DatabaseException { super.initNew(env,locker,databaseName,dbConfig); init(locker); }

	 void initNew( Environment env, Locker locker, String databaseName, DatabaseConfig dbConfig) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	initNew__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void initExisting__wrappee__base( Environment env, Locker locker, DatabaseImpl database, DatabaseConfig dbConfig) throws DatabaseException { Database otherPriDb=database.findPrimaryDatabase(); if (otherPriDb != null && otherPriDb.getDatabaseImpl() != primaryDb.getDatabaseImpl()) { throw new IllegalArgumentException("Secondary is already associated with a different primary: " + otherPriDb.getDebugName()); } super.initExisting(env,locker,database,dbConfig); init(locker); }

	 void initExisting( Environment env, Locker locker, DatabaseImpl database, DatabaseConfig dbConfig) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	initExisting__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void init__wrappee__base( Locker locker) throws DatabaseException { secondaryConfig=(SecondaryConfig)configuration; primaryDb.addTrigger(secondaryTrigger,false); Database foreignDb=secondaryConfig.getForeignKeyDatabase(); if (foreignDb != null) { foreignDb.addTrigger(foreignKeyTrigger,true); } if (secondaryConfig.getAllowPopulate()) { Cursor secCursor=null; Cursor priCursor=null; try { secCursor=new Cursor(this,locker,null); DatabaseEntry key=new DatabaseEntry(); DatabaseEntry data=new DatabaseEntry(); OperationStatus status=secCursor.position(key,data,LockMode.DEFAULT,true); if (status == OperationStatus.NOTFOUND) { priCursor=new Cursor(primaryDb,locker,null); status=priCursor.position(key,data,LockMode.DEFAULT,true); while (status == OperationStatus.SUCCESS) { updateSecondary(locker,secCursor,key,null,data); status=priCursor.retrieveNext(key,data,LockMode.DEFAULT,GetMode.NEXT); } } } finally { if (secCursor != null) { secCursor.close(); } if (priCursor != null) { priCursor.close(); } } } }

	 private void init( Locker locker) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	init__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public synchronized void close__wrappee__base() throws DatabaseException { if (primaryDb != null && secondaryTrigger != null) { primaryDb.removeTrigger(secondaryTrigger); } Database foreignDb=secondaryConfig.getForeignKeyDatabase(); if (foreignDb != null && foreignKeyTrigger != null) { foreignDb.removeTrigger(foreignKeyTrigger); } super.close(); }

	 public synchronized void close() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	close__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void clearPrimary__wrappee__base(){ primaryDb=null; secondaryTrigger=null; }

	 void clearPrimary(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	clearPrimary__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void clearForeignKeyTrigger__wrappee__base(){ foreignKeyTrigger=null; }

	 void clearForeignKeyTrigger(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	clearForeignKeyTrigger__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Database getPrimaryDatabase__wrappee__base() throws DatabaseException { return primaryDb; }

	 public Database getPrimaryDatabase() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getPrimaryDatabase__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public SecondaryConfig getSecondaryConfig__wrappee__base() throws DatabaseException { return (SecondaryConfig)getConfig(); }

	 public SecondaryConfig getSecondaryConfig() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getSecondaryConfig__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public SecondaryConfig getPrivateSecondaryConfig__wrappee__base(){ return secondaryConfig; }

	 public SecondaryConfig getPrivateSecondaryConfig(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getPrivateSecondaryConfig__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public SecondaryCursor openSecondaryCursor__wrappee__base( Transaction txn, CursorConfig cursorConfig) throws DatabaseException { return (SecondaryCursor)openCursor(txn,cursorConfig); }

	 public SecondaryCursor openSecondaryCursor( Transaction txn, CursorConfig cursorConfig) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	openSecondaryCursor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 Cursor newDbcInstance__wrappee__base( Transaction txn, CursorConfig cursorConfig) throws DatabaseException { return new SecondaryCursor(this,txn,cursorConfig); }

	 Cursor newDbcInstance( Transaction txn, CursorConfig cursorConfig) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	newDbcInstance__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus delete__wrappee__base( Transaction txn, DatabaseEntry key) throws DatabaseException { checkEnv(); DatabaseUtil.checkForNullDbt(key,"key",true); checkRequiredDbState(OPEN,"Can't call SecondaryDatabase.delete:"); this.hook79(txn,key); Locker locker=null; Cursor cursor=null; OperationStatus commitStatus=OperationStatus.NOTFOUND; try { locker=LockerFactory.getWritableLocker(envHandle,txn,isTransactional()); cursor=new Cursor(this,locker,null); DatabaseEntry pKey=new DatabaseEntry(); OperationStatus searchStatus=cursor.search(key,pKey,LockMode.RMW,SearchMode.SET); if (searchStatus == OperationStatus.SUCCESS) { commitStatus=primaryDb.deleteInternal(locker,pKey); } return commitStatus; } finally { if (cursor != null) { cursor.close(); } if (locker != null) { locker.operationEnd(commitStatus); } } }

	 public OperationStatus delete( Transaction txn, DatabaseEntry key) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	delete__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus get__wrappee__base( Transaction txn, DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { return get(txn,key,new DatabaseEntry(),data,lockMode); }

	 public OperationStatus get( Transaction txn, DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	get__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus get__wrappee__base( Transaction txn, DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkEnv(); DatabaseUtil.checkForNullDbt(key,"key",true); DatabaseUtil.checkForNullDbt(pKey,"pKey",false); DatabaseUtil.checkForNullDbt(data,"data",false); checkRequiredDbState(OPEN,"Can't call SecondaryDatabase.get:"); this.hook80(txn,key,lockMode); CursorConfig cursorConfig=CursorConfig.DEFAULT; if (lockMode == LockMode.READ_COMMITTED) { cursorConfig=CursorConfig.READ_COMMITTED; lockMode=null; } SecondaryCursor cursor=null; try { cursor=new SecondaryCursor(this,txn,cursorConfig); return cursor.search(key,pKey,data,lockMode,SearchMode.SET); } finally { if (cursor != null) { cursor.close(); } } }

	 public OperationStatus get( Transaction txn, DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	get__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getSearchBoth__wrappee__base( Transaction txn, DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { throw notAllowedException(); }

	 public OperationStatus getSearchBoth( Transaction txn, DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getSearchBoth__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getSearchBoth__wrappee__base( Transaction txn, DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkEnv(); DatabaseUtil.checkForNullDbt(key,"key",true); DatabaseUtil.checkForNullDbt(pKey,"pKey",true); DatabaseUtil.checkForNullDbt(data,"data",false); checkRequiredDbState(OPEN,"Can't call SecondaryDatabase.getSearchBoth:"); this.hook81(txn,key,data,lockMode); CursorConfig cursorConfig=CursorConfig.DEFAULT; if (lockMode == LockMode.READ_COMMITTED) { cursorConfig=CursorConfig.READ_COMMITTED; lockMode=null; } SecondaryCursor cursor=null; try { cursor=new SecondaryCursor(this,txn,cursorConfig); return cursor.search(key,pKey,data,lockMode,SearchMode.BOTH); } finally { if (cursor != null) { cursor.close(); } } }

	 public OperationStatus getSearchBoth( Transaction txn, DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getSearchBoth__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus put__wrappee__base( Transaction txn, DatabaseEntry key, DatabaseEntry data) throws DatabaseException { throw notAllowedException(); }

	 public OperationStatus put( Transaction txn, DatabaseEntry key, DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	put__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus putNoOverwrite__wrappee__base( Transaction txn, DatabaseEntry key, DatabaseEntry data) throws DatabaseException { throw notAllowedException(); }

	 public OperationStatus putNoOverwrite( Transaction txn, DatabaseEntry key, DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	putNoOverwrite__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus putNoDupData__wrappee__base( Transaction txn, DatabaseEntry key, DatabaseEntry data) throws DatabaseException { throw notAllowedException(); }

	 public OperationStatus putNoDupData( Transaction txn, DatabaseEntry key, DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	putNoDupData__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public JoinCursor join__wrappee__base( Cursor[] cursors, JoinConfig config) throws DatabaseException { throw notAllowedException(); }

	 public JoinCursor join( Cursor[] cursors, JoinConfig config) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	join__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int truncate__wrappee__base( Transaction txn, boolean countRecords) throws DatabaseException { throw notAllowedException(); }

	 public int truncate( Transaction txn, boolean countRecords) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	truncate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void updateSecondary__wrappee__base( Locker locker, Cursor cursor, DatabaseEntry priKey, DatabaseEntry oldData, DatabaseEntry newData) throws DatabaseException { if (secondaryConfig.getImmutableSecondaryKey() && oldData != null && newData != null) { return; } SecondaryKeyCreator keyCreator=secondaryConfig.getKeyCreator(); if (keyCreator != null) { assert secondaryConfig.getMultiKeyCreator() == null; DatabaseEntry oldSecKey=null; if (oldData != null) { oldSecKey=new DatabaseEntry(); if (!keyCreator.createSecondaryKey(this,priKey,oldData,oldSecKey)) { oldSecKey=null; } } DatabaseEntry newSecKey=null; if (newData != null) { newSecKey=new DatabaseEntry(); if (!keyCreator.createSecondaryKey(this,priKey,newData,newSecKey)) { newSecKey=null; } } if ((oldSecKey != null && !oldSecKey.equals(newSecKey)) || (newSecKey != null && !newSecKey.equals(oldSecKey))) { boolean localCursor=(cursor == null); if (localCursor) { cursor=new Cursor(this,locker,null); } try { if (oldSecKey != null) { deleteKey(cursor,priKey,oldSecKey); } if (newSecKey != null) { insertKey(locker,cursor,priKey,newSecKey); } } finally { if (localCursor && cursor != null) { cursor.close(); } } } } else { SecondaryMultiKeyCreator multiKeyCreator=secondaryConfig.getMultiKeyCreator(); assert multiKeyCreator != null; Set oldKeys=Collections.EMPTY_SET; Set newKeys=Collections.EMPTY_SET; if (oldData != null) { oldKeys=new HashSet(); multiKeyCreator.createSecondaryKeys(this,priKey,oldData,oldKeys); } if (newData != null) { newKeys=new HashSet(); multiKeyCreator.createSecondaryKeys(this,priKey,newData,newKeys); } if (!oldKeys.equals(newKeys)) { boolean localCursor=(cursor == null); if (localCursor) { cursor=new Cursor(this,locker,null); } try { Set oldKeysCopy=oldKeys; if (oldKeys != Collections.EMPTY_SET) { oldKeysCopy=new HashSet(oldKeys); oldKeys.removeAll(newKeys); for (Iterator i=oldKeys.iterator(); i.hasNext(); ) { DatabaseEntry oldKey=(DatabaseEntry)i.next(); deleteKey(cursor,priKey,oldKey); } } if (newKeys != Collections.EMPTY_SET) { newKeys.removeAll(oldKeysCopy); for (Iterator i=newKeys.iterator(); i.hasNext(); ) { DatabaseEntry newKey=(DatabaseEntry)i.next(); insertKey(locker,cursor,priKey,newKey); } } } finally { if (localCursor && cursor != null) { cursor.close(); } } } } }

	 void updateSecondary( Locker locker, Cursor cursor, DatabaseEntry priKey, DatabaseEntry oldData, DatabaseEntry newData) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	updateSecondary__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void deleteKey__wrappee__base( Cursor cursor, DatabaseEntry priKey, DatabaseEntry oldSecKey) throws DatabaseException { OperationStatus status=cursor.search(oldSecKey,priKey,LockMode.RMW,SearchMode.BOTH); if (status == OperationStatus.SUCCESS) { cursor.deleteInternal(); } else { throw new DatabaseException("Secondary " + getDebugName() + " is corrupt: the primary record contains a key"+ " that is not present in the secondary"); } }

	 private void deleteKey( Cursor cursor, DatabaseEntry priKey, DatabaseEntry oldSecKey) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	deleteKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void insertKey__wrappee__base( Locker locker, Cursor cursor, DatabaseEntry priKey, DatabaseEntry newSecKey) throws DatabaseException { Database foreignDb=secondaryConfig.getForeignKeyDatabase(); if (foreignDb != null) { Cursor foreignCursor=null; try { foreignCursor=new Cursor(foreignDb,locker,null); DatabaseEntry tmpData=new DatabaseEntry(); OperationStatus status=foreignCursor.search(newSecKey,tmpData,LockMode.DEFAULT,SearchMode.SET); if (status != OperationStatus.SUCCESS) { throw new DatabaseException("Secondary " + getDebugName() + " foreign key not allowed: it is not"+ " present in the foreign database"); } } finally { if (foreignCursor != null) { foreignCursor.close(); } } } OperationStatus status; if (configuration.getSortedDuplicates()) { status=cursor.putInternal(newSecKey,priKey,PutMode.NODUP); } else { status=cursor.putInternal(newSecKey,priKey,PutMode.NOOVERWRITE); } if (status != OperationStatus.SUCCESS) { throw new DatabaseException("Could not insert secondary key in " + getDebugName() + ' '+ status); } }

	 private void insertKey( Locker locker, Cursor cursor, DatabaseEntry priKey, DatabaseEntry newSecKey) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	insertKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void onForeignKeyDelete__wrappee__base( Locker locker, DatabaseEntry secKey) throws DatabaseException { ForeignKeyDeleteAction deleteAction=secondaryConfig.getForeignKeyDeleteAction(); LockMode lockMode=(deleteAction == ForeignKeyDeleteAction.ABORT) ? LockMode.DEFAULT : LockMode.RMW; DatabaseEntry priKey=new DatabaseEntry(); Cursor cursor=null; OperationStatus status; try { cursor=new Cursor(this,locker,null); status=cursor.search(secKey,priKey,lockMode,SearchMode.SET); while (status == OperationStatus.SUCCESS) { if (deleteAction == ForeignKeyDeleteAction.ABORT) { throw new DatabaseException("Secondary " + getDebugName() + " refers to a foreign key that has been deleted"+ " (ForeignKeyDeleteAction.ABORT)"); } else if (deleteAction == ForeignKeyDeleteAction.CASCADE) { Cursor priCursor=null; try { DatabaseEntry data=new DatabaseEntry(); priCursor=new Cursor(primaryDb,locker,null); status=priCursor.search(priKey,data,LockMode.RMW,SearchMode.SET); if (status == OperationStatus.SUCCESS) { priCursor.delete(); } else { throw secondaryCorruptException(); } } finally { if (priCursor != null) { priCursor.close(); } } } else if (deleteAction == ForeignKeyDeleteAction.NULLIFY) { Cursor priCursor=null; try { DatabaseEntry data=new DatabaseEntry(); priCursor=new Cursor(primaryDb,locker,null); status=priCursor.search(priKey,data,LockMode.RMW,SearchMode.SET); if (status == OperationStatus.SUCCESS) { ForeignMultiKeyNullifier multiNullifier=secondaryConfig.getForeignMultiKeyNullifier(); if (multiNullifier != null) { if (multiNullifier.nullifyForeignKey(this,priKey,data,secKey)) { priCursor.putCurrent(data); } } else { ForeignKeyNullifier nullifier=secondaryConfig.getForeignKeyNullifier(); if (nullifier.nullifyForeignKey(this,data)) { priCursor.putCurrent(data); } } } else { throw secondaryCorruptException(); } } finally { if (priCursor != null) { priCursor.close(); } } } else { throw new IllegalStateException(); } status=cursor.retrieveNext(secKey,priKey,LockMode.DEFAULT,GetMode.NEXT_DUP); } } finally { if (cursor != null) { cursor.close(); } } }

	 void onForeignKeyDelete( Locker locker, DatabaseEntry secKey) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	onForeignKeyDelete__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 DatabaseException secondaryCorruptException__wrappee__base() throws DatabaseException { throw new DatabaseException("Secondary " + getDebugName() + " is corrupt: it refers"+ " to a missing key in the primary database"); }

	 DatabaseException secondaryCorruptException() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	secondaryCorruptException__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 static UnsupportedOperationException notAllowedException__wrappee__base(){ throw new UnsupportedOperationException("Operation not allowed on a secondary"); }

	 static UnsupportedOperationException notAllowedException(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	notAllowedException__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook79__wrappee__base( Transaction txn, DatabaseEntry key) throws DatabaseException { }

	 protected void hook79( Transaction txn, DatabaseEntry key) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook79__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook80__wrappee__base( Transaction txn, DatabaseEntry key, LockMode lockMode) throws DatabaseException { }

	 protected void hook80( Transaction txn, DatabaseEntry key, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook80__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook81__wrappee__base( Transaction txn, DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { }

	 protected void hook81( Transaction txn, DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook81__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
