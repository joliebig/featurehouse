package com.sleepycat.je.dbi; 
import java.io.File; 
import java.io.IOException; 
import java.io.PrintStream; 
import java.util.ArrayList; 
import java.util.Collection; 
import java.util.List; 
import java.util.logging.ConsoleHandler; 
import java.util.logging.FileHandler; 
import java.util.logging.Handler; 
import java.util.logging.Level; 
import java.util.logging.Logger; 
import java.util.logging.SimpleFormatter; 
import com.sleepycat.je.CheckpointConfig; 
import com.sleepycat.je.Database; 
import com.sleepycat.je.DatabaseConfig; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.DbInternal; 
import com.sleepycat.je.EnvironmentConfig; 
import com.sleepycat.je.EnvironmentMutableConfig; 
import com.sleepycat.je.LockStats; 
import com.sleepycat.je.RunRecoveryException; 
import com.sleepycat.je.Transaction; 
import com.sleepycat.je.TransactionConfig; 
import com.sleepycat.je.cleaner.Cleaner; 
import com.sleepycat.je.cleaner.UtilizationProfile; 
import com.sleepycat.je.cleaner.UtilizationTracker; 
import com.sleepycat.je.config.EnvironmentParams; 
import com.sleepycat.je.log.FileManager; 
import com.sleepycat.je.log.LogManager; 
import com.sleepycat.je.log.SyncedLogManager; 
import com.sleepycat.je.recovery.Checkpointer; 
import com.sleepycat.je.recovery.RecoveryInfo; 
import com.sleepycat.je.recovery.RecoveryManager; 
import com.sleepycat.je.tree.BIN; 
import com.sleepycat.je.tree.BINReference; 
import com.sleepycat.je.tree.IN; 
import com.sleepycat.je.tree.Key; 
import com.sleepycat.je.txn.Locker; 
import com.sleepycat.je.txn.Txn; 
import com.sleepycat.je.txn.TxnManager; 
import com.sleepycat.je.utilint.DbLsn; 
import com.sleepycat.je.utilint.PropUtil; 
import com.sleepycat.je.utilint.Tracer; 
import de.ovgu.cide.jakutil.*; 
public  class  EnvironmentImpl  implements EnvConfigObserver {
	 private static final boolean TEST_NO_LOCKING_MODE=false;

	 private DbEnvState envState;

	 private boolean closing;

	 private File envHome;

	 private int referenceCount;

	 private boolean isTransactional;

	 private boolean isNoLocking;

	 private boolean isReadOnly;

	 private MemoryBudget memoryBudget;

	 private long lockTimeout;

	 private long txnTimeout;

	 private DbTree dbMapTree;

	 private long mapTreeRootLsn=DbLsn.NULL_LSN;

	 private INList inMemoryINs;

	 private DbConfigManager configManager;

	 private List configObservers;

	 protected LogManager logManager;

	 private FileManager fileManager;

	 private TxnManager txnManager;

	 private Checkpointer checkpointer;

	 private Cleaner cleaner;

	 private RecoveryInfo lastRecoveryInfo;

	 private RunRecoveryException savedInvalidatingException;

	 private static boolean forcedYield=false;

	 private static int threadLocalReferenceCount=0;

	 private static boolean noComparators=false;

	 public static final boolean JAVA5_AVAILABLE;

	 private static final String DISABLE_JAVA_ADLER32="je.disable.java.adler32";

	
static { boolean ret=false; if (System.getProperty(DISABLE_JAVA_ADLER32) == null) { String javaVersion=System.getProperty("java.version"); if (javaVersion != null && !javaVersion.startsWith("1.4.")) { ret=true; } } JAVA5_AVAILABLE=ret; }

	 public EnvironmentImpl( File envHome, EnvironmentConfig envConfig) throws DatabaseException { try { this.envHome=envHome; envState=DbEnvState.INIT; this.hook323(); configManager=new DbConfigManager(envConfig); configObservers=new ArrayList(); addConfigObserver(this); memoryBudget=new MemoryBudget(this,configManager); this.hook336(envHome); forcedYield=configManager.getBoolean(EnvironmentParams.ENV_FORCED_YIELD); isTransactional=configManager.getBoolean(EnvironmentParams.ENV_INIT_TXN); isNoLocking=!(configManager.getBoolean(EnvironmentParams.ENV_INIT_LOCKING)); if (isTransactional && isNoLocking) { if (TEST_NO_LOCKING_MODE) { isNoLocking=!isTransactional; } else { throw new IllegalArgumentException("Can't set 'je.env.isNoLocking' and " + "'je.env.isTransactional';"); } } this.hook322(); isReadOnly=configManager.getBoolean(EnvironmentParams.ENV_RDONLY); fileManager=new FileManager(this,envHome,isReadOnly); if (!envConfig.getAllowCreate() && !fileManager.filesExist()) { throw new DatabaseException("Enviroment creation isn't allowed, " + " but there is no pre-existing " + " environment in "+ envHome); } this.hook321(); inMemoryINs=new INList(this); txnManager=new TxnManager(this); createDaemons(); dbMapTree=new DbTree(this); referenceCount=0; this.hook320(); if (configManager.getBoolean(EnvironmentParams.ENV_RECOVERY)) { try { RecoveryManager recoveryManager=new RecoveryManager(this); lastRecoveryInfo=recoveryManager.recover(isReadOnly); } finally { try { logManager.flush(); fileManager.clear(); } catch ( IOException e) { throw new DatabaseException(e.getMessage()); } } } else { isReadOnly=true; noComparators=true; } runOrPauseDaemons(configManager); lockTimeout=PropUtil.microsToMillis(configManager.getLong(EnvironmentParams.LOCK_TIMEOUT)); txnTimeout=PropUtil.microsToMillis(configManager.getLong(EnvironmentParams.TXN_TIMEOUT)); this.hook335(); open(); } catch ( DatabaseException e) { if (fileManager != null) { try { fileManager.close(); } catch ( IOException IOE) { } } throw e; } }

	
@MethodObject static  class  EnvironmentImpl_createDaemons {
		 EnvironmentImpl_createDaemons( EnvironmentImpl _this){ this._this=_this; }

		 protected EnvironmentImpl _this;

		 protected long checkpointerWakeupTime;

		 protected long compressorWakeupInterval;

		 void execute__wrappee__base() throws DatabaseException { checkpointerWakeupTime=0; this.hook329(); _this.checkpointer=new Checkpointer(_this,checkpointerWakeupTime,"Checkpointer"); this.hook332(); _this.cleaner=new Cleaner(_this,"Cleaner"); }

		 void execute() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	execute__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook329__wrappee__base() throws DatabaseException { }

		 protected void hook329() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook329__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 protected void hook332__wrappee__base() throws DatabaseException { }

		 protected void hook332() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook332__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	 public void envConfigUpdate__wrappee__base( DbConfigManager mgr) throws DatabaseException { runOrPauseDaemons(mgr); }

	 public void envConfigUpdate( DbConfigManager mgr) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	envConfigUpdate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void createDaemons__wrappee__base() throws DatabaseException { new EnvironmentImpl_createDaemons(this).execute(); }

	 private void createDaemons() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	createDaemons__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void runOrPauseDaemons__wrappee__base( DbConfigManager mgr) throws DatabaseException { if (!isReadOnly) { this.hook330(mgr); this.hook333(mgr); this.hook326(mgr); } this.hook317(mgr); }

	 private void runOrPauseDaemons( DbConfigManager mgr) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	runOrPauseDaemons__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public UtilizationTracker getUtilizationTracker__wrappee__base(){ return cleaner.getUtilizationTracker(); }

	 public UtilizationTracker getUtilizationTracker(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getUtilizationTracker__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public UtilizationProfile getUtilizationProfile__wrappee__base(){ return cleaner.getUtilizationProfile(); }

	 public UtilizationProfile getUtilizationProfile(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getUtilizationProfile__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void logMapTreeRoot__wrappee__base() throws DatabaseException { mapTreeRootLsn=logManager.log(dbMapTree); }

	 public void logMapTreeRoot() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	logMapTreeRoot__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void rewriteMapTreeRoot__wrappee__base( long cleanerTargetLsn) throws DatabaseException { if (DbLsn.compareTo(cleanerTargetLsn,mapTreeRootLsn) == 0) { mapTreeRootLsn=logManager.log(dbMapTree); } }

	 public void rewriteMapTreeRoot( long cleanerTargetLsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	rewriteMapTreeRoot__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getRootLsn__wrappee__base(){ return mapTreeRootLsn; }

	 public long getRootLsn(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getRootLsn__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void readMapTreeFromLog__wrappee__base( long rootLsn) throws DatabaseException { dbMapTree=(DbTree)logManager.get(rootLsn); dbMapTree.setEnvironmentImpl(this); this.hook324(rootLsn); }

	 public void readMapTreeFromLog( long rootLsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readMapTreeFromLog__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void open__wrappee__base(){ envState=DbEnvState.OPEN; }

	 public void open(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	open__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void invalidate__wrappee__base( RunRecoveryException e){ savedInvalidatingException=e; envState=DbEnvState.INVALID; requestShutdownDaemons(); }

	 public void invalidate( RunRecoveryException e){ t.in(Thread.currentThread().getStackTrace()[1].toString());	invalidate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isOpen__wrappee__base(){ return (envState == DbEnvState.OPEN); }

	 public boolean isOpen(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isOpen__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isClosing__wrappee__base(){ return closing; }

	 public boolean isClosing(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isClosing__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isClosed__wrappee__base(){ return (envState == DbEnvState.CLOSED); }

	 public boolean isClosed(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isClosed__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean mayNotWrite__wrappee__base(){ return (envState == DbEnvState.INVALID) || (envState == DbEnvState.CLOSED); }

	 public boolean mayNotWrite(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	mayNotWrite__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void checkIfInvalid__wrappee__base() throws RunRecoveryException { if (envState == DbEnvState.INVALID) { savedInvalidatingException.setAlreadyThrown(); throw savedInvalidatingException; } }

	 public void checkIfInvalid() throws RunRecoveryException { t.in(Thread.currentThread().getStackTrace()[1].toString());	checkIfInvalid__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void checkNotClosed__wrappee__base() throws DatabaseException { if (envState == DbEnvState.CLOSED) { throw new DatabaseException("Attempt to use a Environment that has been closed."); } }

	 public void checkNotClosed() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	checkNotClosed__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public synchronized void close__wrappee__base() throws DatabaseException { if (--referenceCount <= 0) { doClose(true); } }

	 public synchronized void close() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	close__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public synchronized void close__wrappee__base( boolean doCheckpoint) throws DatabaseException { if (--referenceCount <= 0) { doClose(doCheckpoint); } }

	 public synchronized void close( boolean doCheckpoint) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	close__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void doClose__wrappee__base( boolean doCheckpoint) throws DatabaseException { StringBuffer errors=new StringBuffer(); try { this.hook319(); try { envState.checkState(DbEnvState.VALID_FOR_CLOSE,DbEnvState.CLOSED); } catch ( DatabaseException DBE) { throw DBE; } requestShutdownDaemons(); if (doCheckpoint && !isReadOnly && (envState != DbEnvState.INVALID)&& logManager.getLastLsnAtRecovery() != fileManager.getLastUsedLsn()) { CheckpointConfig ckptConfig=new CheckpointConfig(); ckptConfig.setForce(true); ckptConfig.setMinimizeRecoveryTime(true); try { invokeCheckpoint(ckptConfig,false,"close"); } catch ( DatabaseException IE) { errors.append("\nException performing checkpoint: "); errors.append(IE.toString()).append("\n"); } } try { shutdownDaemons(); } catch ( InterruptedException IE) { errors.append("\nException shutting down daemon threads: "); errors.append(IE.toString()).append("\n"); } this.hook318(); try { logManager.flush(); } catch ( DatabaseException DBE) { errors.append("\nException flushing log manager: "); errors.append(DBE.toString()).append("\n"); } try { fileManager.clear(); } catch ( IOException IOE) { errors.append("\nException clearing file manager: "); errors.append(IOE.toString()).append("\n"); }
catch ( DatabaseException DBE) { errors.append("\nException clearing file manager: "); errors.append(DBE.toString()).append("\n"); } try { fileManager.close(); } catch ( IOException IOE) { errors.append("\nException clearing file manager: "); errors.append(IOE.toString()).append("\n"); }
catch ( DatabaseException DBE) { errors.append("\nException clearing file manager: "); errors.append(DBE.toString()).append("\n"); } try { inMemoryINs.clear(); } catch ( DatabaseException DBE) { errors.append("\nException closing file manager: "); errors.append(DBE.toString()).append("\n"); } this.hook337(); DbEnvPool.getInstance().remove(envHome); this.hook325(errors); } finally { envState=DbEnvState.CLOSED; } if (errors.length() > 0 && savedInvalidatingException == null) { throw new RunRecoveryException(this,errors.toString()); } }

	 private void doClose( boolean doCheckpoint) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	doClose__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public synchronized void closeAfterRunRecovery__wrappee__base() throws DatabaseException { try { shutdownDaemons(); } catch ( InterruptedException IE) { } try { fileManager.clear(); } catch ( Exception e) { } try { fileManager.close(); } catch ( Exception e) { } DbEnvPool.getInstance().remove(envHome); }

	 public synchronized void closeAfterRunRecovery() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	closeAfterRunRecovery__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public synchronized void forceClose__wrappee__base() throws DatabaseException { referenceCount=1; close(); }

	 public synchronized void forceClose() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	forceClose__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public synchronized void incReferenceCount__wrappee__base(){ referenceCount++; }

	 public synchronized void incReferenceCount(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	incReferenceCount__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static int getThreadLocalReferenceCount__wrappee__base(){ return threadLocalReferenceCount; }

	 public static int getThreadLocalReferenceCount(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getThreadLocalReferenceCount__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static synchronized void incThreadLocalReferenceCount__wrappee__base(){ threadLocalReferenceCount++; }

	 public static synchronized void incThreadLocalReferenceCount(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	incThreadLocalReferenceCount__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static synchronized void decThreadLocalReferenceCount__wrappee__base(){ threadLocalReferenceCount--; }

	 public static synchronized void decThreadLocalReferenceCount(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	decThreadLocalReferenceCount__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static boolean getNoComparators__wrappee__base(){ return noComparators; }

	 public static boolean getNoComparators(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getNoComparators__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean invokeCheckpoint__wrappee__base( CheckpointConfig config, boolean flushAll, String invokingSource) throws DatabaseException { if (checkpointer != null) { checkpointer.doCheckpoint(config,flushAll,invokingSource); return true; } else { return false; } }

	 public boolean invokeCheckpoint( CheckpointConfig config, boolean flushAll, String invokingSource) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	invokeCheckpoint__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int invokeCleaner__wrappee__base() throws DatabaseException { if (cleaner != null) { return cleaner.doClean(true,false); } else { return 0; } }

	 public int invokeCleaner() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	invokeCleaner__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void requestShutdownDaemons__wrappee__base(){ closing=true; this.hook331(); this.hook334(); this.hook327(); }

	 private void requestShutdownDaemons(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	requestShutdownDaemons__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void shutdownDaemons__wrappee__base() throws InterruptedException { shutdownCheckpointer(); }

	 private void shutdownDaemons() throws InterruptedException { t.in(Thread.currentThread().getStackTrace()[1].toString());	shutdownDaemons__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void shutdownCheckpointer__wrappee__base() throws InterruptedException { if (checkpointer != null) { this.hook328(); checkpointer=null; } return; }

	 void shutdownCheckpointer() throws InterruptedException { t.in(Thread.currentThread().getStackTrace()[1].toString());	shutdownCheckpointer__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isNoLocking__wrappee__base(){ return isNoLocking; }

	 public boolean isNoLocking(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isNoLocking__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isTransactional__wrappee__base(){ return isTransactional; }

	 public boolean isTransactional(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isTransactional__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean isReadOnly__wrappee__base(){ return isReadOnly; }

	 public boolean isReadOnly(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isReadOnly__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public DatabaseImpl createDb__wrappee__base( Locker locker, String databaseName, DatabaseConfig dbConfig, Database databaseHandle) throws DatabaseException { return dbMapTree.createDb(locker,databaseName,dbConfig,databaseHandle); }

	 public DatabaseImpl createDb( Locker locker, String databaseName, DatabaseConfig dbConfig, Database databaseHandle) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	createDb__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public DatabaseImpl getDb__wrappee__base( Locker locker, String databaseName, Database databaseHandle) throws DatabaseException { return dbMapTree.getDb(locker,databaseName,databaseHandle); }

	 public DatabaseImpl getDb( Locker locker, String databaseName, Database databaseHandle) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getDb__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public List getDbNames__wrappee__base() throws DatabaseException { return dbMapTree.getDbNames(); }

	 public List getDbNames() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getDbNames__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void dumpMapTree__wrappee__base() throws DatabaseException { dbMapTree.dump(); }

	 public void dumpMapTree() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpMapTree__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Txn txnBegin__wrappee__base( Transaction parent, TransactionConfig txnConfig) throws DatabaseException { if (!isTransactional) { throw new DatabaseException("beginTransaction called, " + " but Environment was not opened " + "with transactional cpabilities"); } return txnManager.txnBegin(parent,txnConfig); }

	 public Txn txnBegin( Transaction parent, TransactionConfig txnConfig) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	txnBegin__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public LogManager getLogManager__wrappee__base(){ return logManager; }

	 public LogManager getLogManager(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLogManager__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public FileManager getFileManager__wrappee__base(){ return fileManager; }

	 public FileManager getFileManager(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getFileManager__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public DbTree getDbMapTree__wrappee__base(){ return dbMapTree; }

	 public DbTree getDbMapTree(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDbMapTree__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public DbConfigManager getConfigManager__wrappee__base(){ return configManager; }

	 public DbConfigManager getConfigManager(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getConfigManager__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public EnvironmentConfig cloneConfig__wrappee__base(){ return DbInternal.cloneConfig(configManager.getEnvironmentConfig()); }

	 public EnvironmentConfig cloneConfig(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	cloneConfig__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public EnvironmentMutableConfig cloneMutableConfig__wrappee__base(){ return DbInternal.cloneMutableConfig(configManager.getEnvironmentConfig()); }

	 public EnvironmentMutableConfig cloneMutableConfig(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	cloneMutableConfig__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void checkImmutablePropsForEquality__wrappee__base( EnvironmentConfig config) throws IllegalArgumentException { DbInternal.checkImmutablePropsForEquality(configManager.getEnvironmentConfig(),config); }

	 public void checkImmutablePropsForEquality( EnvironmentConfig config) throws IllegalArgumentException { t.in(Thread.currentThread().getStackTrace()[1].toString());	checkImmutablePropsForEquality__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public synchronized void setMutableConfig__wrappee__base( EnvironmentMutableConfig config) throws DatabaseException { EnvironmentConfig newConfig=DbInternal.cloneConfig(configManager.getEnvironmentConfig()); DbInternal.copyMutablePropsTo(config,newConfig); configManager=new DbConfigManager(newConfig); for (int i=configObservers.size() - 1; i >= 0; i-=1) { EnvConfigObserver o=(EnvConfigObserver)configObservers.get(i); o.envConfigUpdate(configManager); } }

	 public synchronized void setMutableConfig( EnvironmentMutableConfig config) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	setMutableConfig__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public synchronized void addConfigObserver__wrappee__base( EnvConfigObserver o){ configObservers.add(o); }

	 public synchronized void addConfigObserver( EnvConfigObserver o){ t.in(Thread.currentThread().getStackTrace()[1].toString());	addConfigObserver__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public synchronized void removeConfigObserver__wrappee__base( EnvConfigObserver o){ configObservers.remove(o); }

	 public synchronized void removeConfigObserver( EnvConfigObserver o){ t.in(Thread.currentThread().getStackTrace()[1].toString());	removeConfigObserver__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public INList getInMemoryINs__wrappee__base(){ return inMemoryINs; }

	 public INList getInMemoryINs(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getInMemoryINs__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public TxnManager getTxnManager__wrappee__base(){ return txnManager; }

	 public TxnManager getTxnManager(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTxnManager__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Checkpointer getCheckpointer__wrappee__base(){ return checkpointer; }

	 public Checkpointer getCheckpointer(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getCheckpointer__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Cleaner getCleaner__wrappee__base(){ return cleaner; }

	 public Cleaner getCleaner(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getCleaner__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public MemoryBudget getMemoryBudget__wrappee__base(){ return memoryBudget; }

	 public MemoryBudget getMemoryBudget(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getMemoryBudget__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public RecoveryInfo getLastRecoveryInfo__wrappee__base(){ return lastRecoveryInfo; }

	 public RecoveryInfo getLastRecoveryInfo(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLastRecoveryInfo__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public File getEnvironmentHome__wrappee__base(){ return envHome; }

	 public File getEnvironmentHome(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getEnvironmentHome__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getTxnTimeout__wrappee__base(){ return txnTimeout; }

	 public long getTxnTimeout(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTxnTimeout__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public long getLockTimeout__wrappee__base(){ return lockTimeout; }

	 public long getLockTimeout(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getLockTimeout__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static boolean maybeForceYield__wrappee__base(){ if (forcedYield) { Thread.yield(); } return true; }

	 public static boolean maybeForceYield(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	maybeForceYield__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook317__wrappee__base( DbConfigManager mgr) throws DatabaseException { }

	 protected void hook317( DbConfigManager mgr) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook317__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook318__wrappee__base() throws DatabaseException { }

	 protected void hook318() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook318__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook319__wrappee__base() throws DatabaseException { }

	 protected void hook319() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook319__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook320__wrappee__base() throws DatabaseException { }

	 protected void hook320() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook320__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook321__wrappee__base() throws DatabaseException { logManager=new SyncedLogManager(this,isReadOnly); }

	 protected void hook321() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook321__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook322__wrappee__base() throws DatabaseException { }

	 protected void hook322() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook322__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook323__wrappee__base() throws DatabaseException { }

	 protected void hook323() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook323__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook324__wrappee__base( long rootLsn) throws DatabaseException { mapTreeRootLsn=rootLsn; }

	 protected void hook324( long rootLsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook324__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook325__wrappee__base( StringBuffer errors) throws DatabaseException { }

	 protected void hook325( StringBuffer errors) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook325__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook326__wrappee__base( DbConfigManager mgr) throws DatabaseException { }

	 protected void hook326( DbConfigManager mgr) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook326__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook327__wrappee__base(){ }

	 protected void hook327(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook327__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook328__wrappee__base() throws InterruptedException { }

	 protected void hook328() throws InterruptedException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook328__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook330__wrappee__base( DbConfigManager mgr) throws DatabaseException { }

	 protected void hook330( DbConfigManager mgr) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook330__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook331__wrappee__base(){ }

	 protected void hook331(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook331__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook333__wrappee__base( DbConfigManager mgr) throws DatabaseException { }

	 protected void hook333( DbConfigManager mgr) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook333__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook334__wrappee__base(){ }

	 protected void hook334(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook334__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook335__wrappee__base() throws DatabaseException { }

	 protected void hook335() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook335__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook336__wrappee__base( File envHome) throws DatabaseException { }

	 protected void hook336( File envHome) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook336__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook337__wrappee__base() throws DatabaseException { }

	 protected void hook337() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook337__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
