package com.sleepycat.je; 
import java.util.ArrayList; 
import java.util.Comparator; 
import java.util.Iterator; 
import java.util.List; 
import java.util.logging.Level; 
import java.util.logging.Logger; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.dbi.GetMode; 
import com.sleepycat.je.dbi.PutMode; 
import com.sleepycat.je.dbi.CursorImpl.SearchMode; 
import com.sleepycat.je.txn.Locker; 
import com.sleepycat.je.txn.LockerFactory; 
import com.sleepycat.je.utilint.TinyHashSet; 
import com.sleepycat.je.utilint.Tracer; 
import de.ovgu.cide.jakutil.*; 
public  class  Database {
	
static  class  DbState {
		 private String stateName;

		 DbState( String stateName){ this.stateName=stateName; }

		 public String toString__wrappee__base(){ return "DbState." + stateName; }

		 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	 static DbState OPEN=new DbState("OPEN");

	 static DbState CLOSED=new DbState("CLOSED");

	 static DbState INVALID=new DbState("INVALID");

	 private DbState state;

	 Environment envHandle;

	 private DatabaseImpl databaseImpl;

	 DatabaseConfig configuration;

	 private boolean isWritable;

	 Locker handleLocker;

	 private TinyHashSet cursors=new TinyHashSet();

	 private List triggerList;

	 protected Database( Environment env){ this.envHandle=env; handleLocker=null; }

	
@MethodObject static  class  Database_acquireTriggerListReadLock {
		 Database_acquireTriggerListReadLock( Database _this){ this._this=_this; }

		 protected Database _this;

		 protected EnvironmentImpl env;

		 void execute__wrappee__base() throws DatabaseException { if (_this.triggerList == null) { _this.triggerList=new ArrayList(); } }

		 void execute() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
@MethodObject static  class  Database_acquireTriggerListWriteLock {
		 Database_acquireTriggerListWriteLock( Database _this){ this._this=_this; }

		 protected Database _this;

		 protected EnvironmentImpl env;

		 void execute__wrappee__base() throws DatabaseException { if (_this.triggerList == null) { _this.triggerList=new ArrayList(); } }

		 void execute() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	
@MethodObject static  class  Database_releaseTriggerListWriteLock {
		 Database_releaseTriggerListWriteLock( Database _this){ this._this=_this; }

		 protected Database _this;

		 protected EnvironmentImpl env;

		 void execute__wrappee__base() throws DatabaseException { if (_this.triggerList.size() == 0) { _this.triggerList=null; } }

		 void execute() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	 void initNew__wrappee__base( Environment env, Locker locker, String databaseName, DatabaseConfig dbConfig) throws DatabaseException { if (dbConfig.getReadOnly()) { throw new DatabaseException("DatabaseConfig.setReadOnly() must be set to false " + "when creating a Database"); } init(env,dbConfig); EnvironmentImpl environmentImpl=DbInternal.envGetEnvironmentImpl(envHandle); databaseImpl=environmentImpl.createDb(locker,databaseName,dbConfig,this); databaseImpl.addReferringHandle(this); }

	 void initNew( Environment env, Locker locker, String databaseName, DatabaseConfig dbConfig) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	initNew__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void initExisting__wrappee__base( Environment env, Locker locker, DatabaseImpl databaseImpl, DatabaseConfig dbConfig) throws DatabaseException { validateConfigAgainstExistingDb(dbConfig,databaseImpl); init(env,dbConfig); this.databaseImpl=databaseImpl; databaseImpl.addReferringHandle(this); configuration.setSortedDuplicates(databaseImpl.getSortedDuplicates()); configuration.setTransactional(databaseImpl.isTransactional()); }

	 void initExisting( Environment env, Locker locker, DatabaseImpl databaseImpl, DatabaseConfig dbConfig) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	initExisting__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void init__wrappee__base( Environment env, DatabaseConfig config) throws DatabaseException { handleLocker=null; envHandle=env; configuration=config.cloneConfig(); isWritable=!configuration.getReadOnly(); state=OPEN; }

	 private void init( Environment env, DatabaseConfig config) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	init__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void validateConfigAgainstExistingDb__wrappee__base( DatabaseConfig config, DatabaseImpl databaseImpl) throws DatabaseException { if (!config.getUseExistingConfig()) { if (databaseImpl.getSortedDuplicates() != config.getSortedDuplicates()) { throw new DatabaseException("You can't open a Database with a duplicatesAllowed " + "configuration of " + config.getSortedDuplicates() + " if the underlying database was created with a "+ "duplicatesAllowedSetting of "+ databaseImpl.getSortedDuplicates()+ "."); } } if (databaseImpl.hasOpenHandles()) { if (!config.getUseExistingConfig()) { if (config.getTransactional() != databaseImpl.isTransactional()) { throw new DatabaseException("You can't open a Database with a transactional " + "configuration of " + config.getTransactional() + " if the underlying database was created with a "+ "transactional configuration of "+ databaseImpl.isTransactional()+ "."); } } } else { databaseImpl.setTransactional(config.getTransactional()); } if (config.getOverrideBtreeComparator()) { databaseImpl.setBtreeComparator(config.getBtreeComparator()); } if (config.getOverrideDuplicateComparator()) { databaseImpl.setDuplicateComparator(config.getDuplicateComparator()); } }

	 private void validateConfigAgainstExistingDb( DatabaseConfig config, DatabaseImpl databaseImpl) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	validateConfigAgainstExistingDb__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public synchronized void close__wrappee__base() throws DatabaseException { StringBuffer errors=null; checkEnv(); checkProhibitedDbState(CLOSED,"Can't close Database:"); this.hook44(); removeAllTriggers(); envHandle.removeReferringHandle(this); if (cursors.size() > 0) { errors=new StringBuffer("There are open cursors against the database.\n"); errors.append("They will be closed.\n"); Iterator iter=cursors.copy().iterator(); while (iter.hasNext()) { Cursor dbc=(Cursor)iter.next(); try { dbc.close(); } catch ( DatabaseException DBE) { errors.append("Exception while closing cursors:\n"); errors.append(DBE.toString()); } } } if (databaseImpl != null) { databaseImpl.removeReferringHandle(this); databaseImpl=null; handleLocker.setHandleLockOwner(true,this,true); handleLocker.operationEnd(true); state=CLOSED; } if (errors != null) { throw new DatabaseException(errors.toString()); } }

	 public synchronized void close() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	close__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Sequence openSequence__wrappee__base( Transaction txn, DatabaseEntry key, SequenceConfig config) throws DatabaseException { checkEnv(); DatabaseUtil.checkForNullDbt(key,"key",true); checkRequiredDbState(OPEN,"Can't call Database.openSequence:"); checkWritable("openSequence"); this.hook45(txn,key); return new Sequence(this,txn,key,config); }

	 public Sequence openSequence( Transaction txn, DatabaseEntry key, SequenceConfig config) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	openSequence__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void removeSequence__wrappee__base( Transaction txn, DatabaseEntry key) throws DatabaseException { delete(txn,key); }

	 public void removeSequence( Transaction txn, DatabaseEntry key) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	removeSequence__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public synchronized Cursor openCursor__wrappee__base( Transaction txn, CursorConfig cursorConfig) throws DatabaseException { checkEnv(); checkRequiredDbState(OPEN,"Can't open a cursor"); CursorConfig useConfig=(cursorConfig == null) ? CursorConfig.DEFAULT : cursorConfig; if (useConfig.getReadUncommitted() && useConfig.getReadCommitted()) { throw new IllegalArgumentException("Only one may be specified: ReadCommitted or ReadUncommitted"); } this.hook46(txn,cursorConfig); Cursor ret=newDbcInstance(txn,useConfig); return ret; }

	 public synchronized Cursor openCursor( Transaction txn, CursorConfig cursorConfig) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	openCursor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 Cursor newDbcInstance__wrappee__base( Transaction txn, CursorConfig cursorConfig) throws DatabaseException { return new Cursor(this,txn,cursorConfig); }

	 Cursor newDbcInstance( Transaction txn, CursorConfig cursorConfig) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	newDbcInstance__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus delete__wrappee__base( Transaction txn, DatabaseEntry key) throws DatabaseException { checkEnv(); DatabaseUtil.checkForNullDbt(key,"key",true); checkRequiredDbState(OPEN,"Can't call Database.delete:"); checkWritable("delete"); this.hook47(txn,key); OperationStatus commitStatus=OperationStatus.NOTFOUND; Locker locker=null; try { locker=LockerFactory.getWritableLocker(envHandle,txn,isTransactional()); commitStatus=deleteInternal(locker,key); return commitStatus; } finally { if (locker != null) { locker.operationEnd(commitStatus); } } }

	 public OperationStatus delete( Transaction txn, DatabaseEntry key) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	delete__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 OperationStatus deleteInternal__wrappee__base( Locker locker, DatabaseEntry key) throws DatabaseException { Cursor cursor=null; try { cursor=new Cursor(this,locker,null); cursor.setNonCloning(true); OperationStatus commitStatus=OperationStatus.NOTFOUND; DatabaseEntry oldData=new DatabaseEntry(); OperationStatus searchStatus=cursor.search(key,oldData,LockMode.RMW,SearchMode.SET); if (searchStatus == OperationStatus.SUCCESS) { do { if (hasTriggers()) { notifyTriggers(locker,key,oldData,null); } commitStatus=cursor.deleteNoNotify(); if (commitStatus != OperationStatus.SUCCESS) { return commitStatus; } if (databaseImpl.getSortedDuplicates()) { searchStatus=cursor.retrieveNext(key,oldData,LockMode.RMW,GetMode.NEXT_DUP); } else { searchStatus=OperationStatus.NOTFOUND; } } while (searchStatus == OperationStatus.SUCCESS); commitStatus=OperationStatus.SUCCESS; } return commitStatus; } finally { if (cursor != null) { cursor.close(); } } }

	 OperationStatus deleteInternal( Locker locker, DatabaseEntry key) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	deleteInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus get__wrappee__base( Transaction txn, DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkEnv(); DatabaseUtil.checkForNullDbt(key,"key",true); DatabaseUtil.checkForNullDbt(data,"data",false); checkRequiredDbState(OPEN,"Can't call Database.get:"); this.hook48(txn,key,lockMode); CursorConfig cursorConfig=CursorConfig.DEFAULT; if (lockMode == LockMode.READ_COMMITTED) { cursorConfig=CursorConfig.READ_COMMITTED; lockMode=null; } Cursor cursor=null; try { cursor=new Cursor(this,txn,cursorConfig); cursor.setNonCloning(true); return cursor.search(key,data,lockMode,SearchMode.SET); } finally { if (cursor != null) { cursor.close(); } } }

	 public OperationStatus get( Transaction txn, DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	get__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus getSearchBoth__wrappee__base( Transaction txn, DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { checkEnv(); DatabaseUtil.checkForNullDbt(key,"key",true); DatabaseUtil.checkForNullDbt(data,"data",true); checkRequiredDbState(OPEN,"Can't call Database.getSearchBoth:"); this.hook49(txn,key,data,lockMode); CursorConfig cursorConfig=CursorConfig.DEFAULT; if (lockMode == LockMode.READ_COMMITTED) { cursorConfig=CursorConfig.READ_COMMITTED; lockMode=null; } Cursor cursor=null; try { cursor=new Cursor(this,txn,cursorConfig); cursor.setNonCloning(true); return cursor.search(key,data,lockMode,SearchMode.BOTH); } finally { if (cursor != null) { cursor.close(); } } }

	 public OperationStatus getSearchBoth( Transaction txn, DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getSearchBoth__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus put__wrappee__base( Transaction txn, DatabaseEntry key, DatabaseEntry data) throws DatabaseException { checkEnv(); DatabaseUtil.checkForNullDbt(key,"key",true); DatabaseUtil.checkForNullDbt(data,"data",true); DatabaseUtil.checkForPartialKey(key); checkRequiredDbState(OPEN,"Can't call Database.put"); checkWritable("put"); this.hook50(txn,key,data); return putInternal(txn,key,data,PutMode.OVERWRITE); }

	 public OperationStatus put( Transaction txn, DatabaseEntry key, DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	put__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus putNoOverwrite__wrappee__base( Transaction txn, DatabaseEntry key, DatabaseEntry data) throws DatabaseException { checkEnv(); DatabaseUtil.checkForNullDbt(key,"key",true); DatabaseUtil.checkForNullDbt(data,"data",true); DatabaseUtil.checkForPartialKey(key); checkRequiredDbState(OPEN,"Can't call Database.putNoOverWrite"); checkWritable("putNoOverwrite"); this.hook51(txn,key,data); return putInternal(txn,key,data,PutMode.NOOVERWRITE); }

	 public OperationStatus putNoOverwrite( Transaction txn, DatabaseEntry key, DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	putNoOverwrite__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public OperationStatus putNoDupData__wrappee__base( Transaction txn, DatabaseEntry key, DatabaseEntry data) throws DatabaseException { checkEnv(); DatabaseUtil.checkForNullDbt(key,"key",true); DatabaseUtil.checkForNullDbt(data,"data",true); DatabaseUtil.checkForPartialKey(key); checkRequiredDbState(OPEN,"Can't call Database.putNoDupData"); checkWritable("putNoDupData"); this.hook52(txn,key,data); return putInternal(txn,key,data,PutMode.NODUP); }

	 public OperationStatus putNoDupData( Transaction txn, DatabaseEntry key, DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	putNoDupData__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 OperationStatus putInternal__wrappee__base( Transaction txn, DatabaseEntry key, DatabaseEntry data, PutMode putMode) throws DatabaseException { Locker locker=null; Cursor cursor=null; OperationStatus commitStatus=OperationStatus.KEYEXIST; try { locker=LockerFactory.getWritableLocker(envHandle,txn,isTransactional()); cursor=new Cursor(this,locker,null); cursor.setNonCloning(true); commitStatus=cursor.putInternal(key,data,putMode); return commitStatus; } finally { if (cursor != null) { cursor.close(); } if (locker != null) { locker.operationEnd(commitStatus); } } }

	 OperationStatus putInternal( Transaction txn, DatabaseEntry key, DatabaseEntry data, PutMode putMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	putInternal__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public JoinCursor join__wrappee__base( Cursor[] cursors, JoinConfig config) throws DatabaseException { checkEnv(); checkRequiredDbState(OPEN,"Can't call Database.join"); DatabaseUtil.checkForNullParam(cursors,"cursors"); if (cursors.length == 0) { throw new IllegalArgumentException("At least one cursor is required."); } Locker locker=cursors[0].getCursorImpl().getLocker(); if (!locker.isTransactional()) { EnvironmentImpl env=envHandle.getEnvironmentImpl(); for (int i=1; i < cursors.length; i+=1) { Locker locker2=cursors[i].getCursorImpl().getLocker(); if (locker2.isTransactional()) { throw new IllegalArgumentException("All cursors must use the same transaction."); } EnvironmentImpl env2=cursors[i].getDatabaseImpl().getDbEnvironment(); if (env != env2) { throw new IllegalArgumentException("All cursors must use the same environment."); } } locker=null; } else { for (int i=1; i < cursors.length; i+=1) { Locker locker2=cursors[i].getCursorImpl().getLocker(); if (locker.getTxnLocker() != locker2.getTxnLocker()) { throw new IllegalArgumentException("All cursors must use the same transaction."); } } } return new JoinCursor(locker,this,cursors,config); }

	 public JoinCursor join( Cursor[] cursors, JoinConfig config) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	join__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void preload__wrappee__base( long maxBytes) throws DatabaseException { checkEnv(); checkRequiredDbState(OPEN,"Can't call Database.preload"); this.hook55(); PreloadConfig config=new PreloadConfig(); config.setMaxBytes(maxBytes); databaseImpl.preload(config); }

	 public void preload( long maxBytes) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	preload__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void preload__wrappee__base( long maxBytes, long maxMillisecs) throws DatabaseException { checkEnv(); checkRequiredDbState(OPEN,"Can't call Database.preload"); this.hook56(); PreloadConfig config=new PreloadConfig(); config.setMaxBytes(maxBytes); config.setMaxMillisecs(maxMillisecs); databaseImpl.preload(config); }

	 public void preload( long maxBytes, long maxMillisecs) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	preload__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public PreloadStats preload__wrappee__base( PreloadConfig config) throws DatabaseException { checkEnv(); checkRequiredDbState(OPEN,"Can't call Database.preload"); this.hook57(); return databaseImpl.preload(config); }

	 public PreloadStats preload( PreloadConfig config) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	preload__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public String getDatabaseName__wrappee__base() throws DatabaseException { checkEnv(); if (databaseImpl != null) { return databaseImpl.getName(); } else { return null; } }

	 public String getDatabaseName() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getDatabaseName__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 String getDebugName__wrappee__base(){ if (databaseImpl != null) { return databaseImpl.getDebugName(); } else { return null; } }

	 String getDebugName(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDebugName__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public DatabaseConfig getConfig__wrappee__base() throws DatabaseException { DatabaseConfig showConfig=configuration.cloneConfig(); Comparator btComp=(databaseImpl == null ? null : databaseImpl.getBtreeComparator()); Comparator dupComp=(databaseImpl == null ? null : databaseImpl.getDuplicateComparator()); showConfig.setBtreeComparator(btComp == null ? null : btComp.getClass()); showConfig.setDuplicateComparator(dupComp == null ? null : dupComp.getClass()); return showConfig; }

	 public DatabaseConfig getConfig() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getConfig__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean isTransactional__wrappee__base() throws DatabaseException { return databaseImpl.isTransactional(); }

	 boolean isTransactional() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	isTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Environment getEnvironment__wrappee__base() throws DatabaseException { return envHandle; }

	 public Environment getEnvironment() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getEnvironment__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public List getSecondaryDatabases__wrappee__base() throws DatabaseException { List list=new ArrayList(); if (hasTriggers()) { acquireTriggerListReadLock(); this.hook53(list); } else { } return list; }

	 public List getSecondaryDatabases() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getSecondaryDatabases__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean isWritable__wrappee__base(){ return isWritable; }

	 boolean isWritable(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isWritable__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 DatabaseImpl getDatabaseImpl__wrappee__base(){ return databaseImpl; }

	 DatabaseImpl getDatabaseImpl(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDatabaseImpl__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void setHandleLocker__wrappee__base( Locker locker){ handleLocker=locker; }

	 void setHandleLocker( Locker locker){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setHandleLocker__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 synchronized void removeCursor__wrappee__base( Cursor dbc){ cursors.remove(dbc); }

	 synchronized void removeCursor( Cursor dbc){ t.in(Thread.currentThread().getStackTrace()[1].toString());	removeCursor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 synchronized void addCursor__wrappee__base( Cursor dbc){ cursors.add(dbc); }

	 synchronized void addCursor( Cursor dbc){ t.in(Thread.currentThread().getStackTrace()[1].toString());	addCursor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void checkRequiredDbState__wrappee__base( DbState required, String msg) throws DatabaseException { if (state != required) { throw new DatabaseException(msg + " Database state can't be " + state+ " must be "+ required); } }

	 void checkRequiredDbState( DbState required, String msg) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	checkRequiredDbState__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void checkProhibitedDbState__wrappee__base( DbState prohibited, String msg) throws DatabaseException { if (state == prohibited) { throw new DatabaseException(msg + " Database state must not be " + prohibited); } }

	 void checkProhibitedDbState( DbState prohibited, String msg) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	checkProhibitedDbState__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void checkEnv__wrappee__base() throws RunRecoveryException { EnvironmentImpl env=envHandle.getEnvironmentImpl(); if (env != null) { env.checkIfInvalid(); } }

	 void checkEnv() throws RunRecoveryException { t.in(Thread.currentThread().getStackTrace()[1].toString());	checkEnv__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 synchronized void invalidate__wrappee__base(){ state=INVALID; envHandle.removeReferringHandle(this); if (databaseImpl != null) { databaseImpl.removeReferringHandle(this); } }

	 synchronized void invalidate(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	invalidate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void checkWritable__wrappee__base( String operation) throws DatabaseException { if (!isWritable) { throw new DatabaseException("Database is Read Only: " + operation); } }

	 private void checkWritable( String operation) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	checkWritable__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean hasTriggers__wrappee__base(){ return triggerList != null; }

	 boolean hasTriggers(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hasTriggers__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void acquireTriggerListReadLock__wrappee__base() throws DatabaseException { new Database_acquireTriggerListReadLock(this).execute(); }

	 private void acquireTriggerListReadLock() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	acquireTriggerListReadLock__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void acquireTriggerListWriteLock__wrappee__base() throws DatabaseException { new Database_acquireTriggerListWriteLock(this).execute(); }

	 private void acquireTriggerListWriteLock() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	acquireTriggerListWriteLock__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void releaseTriggerListWriteLock__wrappee__base() throws DatabaseException { new Database_releaseTriggerListWriteLock(this).execute(); }

	 private void releaseTriggerListWriteLock() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	releaseTriggerListWriteLock__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void addTrigger__wrappee__base( DatabaseTrigger trigger, boolean insertAtFront) throws DatabaseException { acquireTriggerListWriteLock(); try { if (insertAtFront) { triggerList.add(0,trigger); } else { triggerList.add(trigger); } trigger.triggerAdded(this); } finally { releaseTriggerListWriteLock(); } }

	 void addTrigger( DatabaseTrigger trigger, boolean insertAtFront) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	addTrigger__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void removeTrigger__wrappee__base( DatabaseTrigger trigger) throws DatabaseException { acquireTriggerListWriteLock(); try { triggerList.remove(trigger); trigger.triggerRemoved(this); } finally { releaseTriggerListWriteLock(); } }

	 void removeTrigger( DatabaseTrigger trigger) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	removeTrigger__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void removeAllTriggers__wrappee__base() throws DatabaseException { acquireTriggerListWriteLock(); try { for (int i=0; i < triggerList.size(); i+=1) { DatabaseTrigger trigger=(DatabaseTrigger)triggerList.get(i); trigger.triggerRemoved(this); } triggerList.clear(); } finally { releaseTriggerListWriteLock(); } }

	 private void removeAllTriggers() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	removeAllTriggers__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void notifyTriggers__wrappee__base( Locker locker, DatabaseEntry priKey, DatabaseEntry oldData, DatabaseEntry newData) throws DatabaseException { acquireTriggerListReadLock(); this.hook54(locker,priKey,oldData,newData); }

	 void notifyTriggers( Locker locker, DatabaseEntry priKey, DatabaseEntry oldData, DatabaseEntry newData) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	notifyTriggers__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook44__wrappee__base() throws DatabaseException { }

	 protected void hook44() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook44__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook45__wrappee__base( Transaction txn, DatabaseEntry key) throws DatabaseException { }

	 protected void hook45( Transaction txn, DatabaseEntry key) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook45__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook46__wrappee__base( Transaction txn, CursorConfig cursorConfig) throws DatabaseException { }

	 protected void hook46( Transaction txn, CursorConfig cursorConfig) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook46__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook47__wrappee__base( Transaction txn, DatabaseEntry key) throws DatabaseException { }

	 protected void hook47( Transaction txn, DatabaseEntry key) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook47__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook48__wrappee__base( Transaction txn, DatabaseEntry key, LockMode lockMode) throws DatabaseException { }

	 protected void hook48( Transaction txn, DatabaseEntry key, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook48__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook49__wrappee__base( Transaction txn, DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { }

	 protected void hook49( Transaction txn, DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook49__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook50__wrappee__base( Transaction txn, DatabaseEntry key, DatabaseEntry data) throws DatabaseException { }

	 protected void hook50( Transaction txn, DatabaseEntry key, DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook50__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook51__wrappee__base( Transaction txn, DatabaseEntry key, DatabaseEntry data) throws DatabaseException { }

	 protected void hook51( Transaction txn, DatabaseEntry key, DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook51__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook52__wrappee__base( Transaction txn, DatabaseEntry key, DatabaseEntry data) throws DatabaseException { }

	 protected void hook52( Transaction txn, DatabaseEntry key, DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook52__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook53__wrappee__base( List list) throws DatabaseException { for (int i=0; i < triggerList.size(); i+=1) { Object obj=triggerList.get(i); if (obj instanceof SecondaryTrigger) { list.add(((SecondaryTrigger)obj).getDb()); } } }

	 protected void hook53( List list) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook53__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook54__wrappee__base( Locker locker, DatabaseEntry priKey, DatabaseEntry oldData, DatabaseEntry newData) throws DatabaseException { for (int i=0; i < triggerList.size(); i+=1) { DatabaseTrigger trigger=(DatabaseTrigger)triggerList.get(i); trigger.databaseUpdated(this,locker,priKey,oldData,newData); } }

	 protected void hook54( Locker locker, DatabaseEntry priKey, DatabaseEntry oldData, DatabaseEntry newData) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook54__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook55__wrappee__base() throws DatabaseException { }

	 protected void hook55() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook55__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook56__wrappee__base() throws DatabaseException { }

	 protected void hook56() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook56__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook57__wrappee__base() throws DatabaseException { }

	 protected void hook57() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook57__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
