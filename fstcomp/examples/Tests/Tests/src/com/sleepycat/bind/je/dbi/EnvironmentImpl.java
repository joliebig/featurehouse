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
public   class  EnvironmentImpl  implements EnvConfigObserver {
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

	 public void envConfigUpdate( DbConfigManager mgr) throws DatabaseException { runOrPauseDaemons(mgr); }

	 private void createDaemons() throws DatabaseException { new EnvironmentImpl_createDaemons(this).execute(); }

	 private void runOrPauseDaemons( DbConfigManager mgr) throws DatabaseException { if (!isReadOnly) { this.hook330(mgr); this.hook333(mgr); this.hook326(mgr); } this.hook317(mgr); }

	 public UtilizationTracker getUtilizationTracker(){ return cleaner.getUtilizationTracker(); }

	 public UtilizationProfile getUtilizationProfile(){ return cleaner.getUtilizationProfile(); }

	 public void logMapTreeRoot() throws DatabaseException { mapTreeRootLsn=logManager.log(dbMapTree); }

	 public void rewriteMapTreeRoot( long cleanerTargetLsn) throws DatabaseException { if (DbLsn.compareTo(cleanerTargetLsn,mapTreeRootLsn) == 0) { mapTreeRootLsn=logManager.log(dbMapTree); } }

	 public long getRootLsn(){ return mapTreeRootLsn; }

	 public void readMapTreeFromLog( long rootLsn) throws DatabaseException { dbMapTree=(DbTree)logManager.get(rootLsn); dbMapTree.setEnvironmentImpl(this); this.hook324(rootLsn); }

	 public void open(){ envState=DbEnvState.OPEN; }

	 public void invalidate( RunRecoveryException e){ savedInvalidatingException=e; envState=DbEnvState.INVALID; requestShutdownDaemons(); }

	 public boolean isOpen(){ return (envState == DbEnvState.OPEN); }

	 public boolean isClosing(){ return closing; }

	 public boolean isClosed(){ return (envState == DbEnvState.CLOSED); }

	 public boolean mayNotWrite(){ return (envState == DbEnvState.INVALID) || (envState == DbEnvState.CLOSED); }

	 public void checkIfInvalid() throws RunRecoveryException { if (envState == DbEnvState.INVALID) { savedInvalidatingException.setAlreadyThrown(); throw savedInvalidatingException; } }

	 public void checkNotClosed() throws DatabaseException { if (envState == DbEnvState.CLOSED) { throw new DatabaseException("Attempt to use a Environment that has been closed."); } }

	 public synchronized void close() throws DatabaseException { if (--referenceCount <= 0) { doClose(true); } }

	 public synchronized void close( boolean doCheckpoint) throws DatabaseException { if (--referenceCount <= 0) { doClose(doCheckpoint); } }

	 private void doClose( boolean doCheckpoint) throws DatabaseException { StringBuffer errors=new StringBuffer(); try { this.hook319(); try { envState.checkState(DbEnvState.VALID_FOR_CLOSE,DbEnvState.CLOSED); } catch ( DatabaseException DBE) { throw DBE; } requestShutdownDaemons(); if (doCheckpoint && !isReadOnly && (envState != DbEnvState.INVALID)&& logManager.getLastLsnAtRecovery() != fileManager.getLastUsedLsn()) { CheckpointConfig ckptConfig=new CheckpointConfig(); ckptConfig.setForce(true); ckptConfig.setMinimizeRecoveryTime(true); try { invokeCheckpoint(ckptConfig,false,"close"); } catch ( DatabaseException IE) { errors.append("\nException performing checkpoint: "); errors.append(IE.toString()).append("\n"); } } try { shutdownDaemons(); } catch ( InterruptedException IE) { errors.append("\nException shutting down daemon threads: "); errors.append(IE.toString()).append("\n"); } this.hook318(); try { logManager.flush(); } catch ( DatabaseException DBE) { errors.append("\nException flushing log manager: "); errors.append(DBE.toString()).append("\n"); } try { fileManager.clear(); } catch ( IOException IOE) { errors.append("\nException clearing file manager: "); errors.append(IOE.toString()).append("\n"); }
catch ( DatabaseException DBE) { errors.append("\nException clearing file manager: "); errors.append(DBE.toString()).append("\n"); } try { fileManager.close(); } catch ( IOException IOE) { errors.append("\nException clearing file manager: "); errors.append(IOE.toString()).append("\n"); }
catch ( DatabaseException DBE) { errors.append("\nException clearing file manager: "); errors.append(DBE.toString()).append("\n"); } try { inMemoryINs.clear(); } catch ( DatabaseException DBE) { errors.append("\nException closing file manager: "); errors.append(DBE.toString()).append("\n"); } this.hook337(); DbEnvPool.getInstance().remove(envHome); this.hook325(errors); } finally { envState=DbEnvState.CLOSED; } if (errors.length() > 0 && savedInvalidatingException == null) { throw new RunRecoveryException(this,errors.toString()); } }

	 public synchronized void closeAfterRunRecovery() throws DatabaseException { try { shutdownDaemons(); } catch ( InterruptedException IE) { } try { fileManager.clear(); } catch ( Exception e) { } try { fileManager.close(); } catch ( Exception e) { } DbEnvPool.getInstance().remove(envHome); }

	 public synchronized void forceClose() throws DatabaseException { referenceCount=1; close(); }

	 public synchronized void incReferenceCount(){ referenceCount++; }

	 public static int getThreadLocalReferenceCount(){ return threadLocalReferenceCount; }

	 public static synchronized void incThreadLocalReferenceCount(){ threadLocalReferenceCount++; }

	 public static synchronized void decThreadLocalReferenceCount(){ threadLocalReferenceCount--; }

	 public static boolean getNoComparators(){ return noComparators; }

	 public boolean invokeCheckpoint( CheckpointConfig config, boolean flushAll, String invokingSource) throws DatabaseException { if (checkpointer != null) { checkpointer.doCheckpoint(config,flushAll,invokingSource); return true; } else { return false; } }

	 public int invokeCleaner() throws DatabaseException { if (cleaner != null) { return cleaner.doClean(true,false); } else { return 0; } }

	 private void requestShutdownDaemons(){ closing=true; this.hook331(); this.hook334(); this.hook327(); }

	 private void shutdownDaemons() throws InterruptedException { shutdownCheckpointer(); }

	 void shutdownCheckpointer() throws InterruptedException { if (checkpointer != null) { this.hook328(); checkpointer=null; } return; }

	 public boolean isNoLocking(){ return isNoLocking; }

	 public boolean isTransactional(){ return isTransactional; }

	 public boolean isReadOnly(){ return isReadOnly; }

	 public DatabaseImpl createDb( Locker locker, String databaseName, DatabaseConfig dbConfig, Database databaseHandle) throws DatabaseException { return dbMapTree.createDb(locker,databaseName,dbConfig,databaseHandle); }

	 public DatabaseImpl getDb( Locker locker, String databaseName, Database databaseHandle) throws DatabaseException { return dbMapTree.getDb(locker,databaseName,databaseHandle); }

	 public List getDbNames() throws DatabaseException { return dbMapTree.getDbNames(); }

	 public void dumpMapTree() throws DatabaseException { dbMapTree.dump(); }

	 public Txn txnBegin( Transaction parent, TransactionConfig txnConfig) throws DatabaseException { if (!isTransactional) { throw new DatabaseException("beginTransaction called, " + " but Environment was not opened " + "with transactional cpabilities"); } return txnManager.txnBegin(parent,txnConfig); }

	 public LogManager getLogManager(){ return logManager; }

	 public FileManager getFileManager(){ return fileManager; }

	 public DbTree getDbMapTree(){ return dbMapTree; }

	 public DbConfigManager getConfigManager(){ return configManager; }

	 public EnvironmentConfig cloneConfig(){ return DbInternal.cloneConfig(configManager.getEnvironmentConfig()); }

	 public EnvironmentMutableConfig cloneMutableConfig(){ return DbInternal.cloneMutableConfig(configManager.getEnvironmentConfig()); }

	 public void checkImmutablePropsForEquality( EnvironmentConfig config) throws IllegalArgumentException { DbInternal.checkImmutablePropsForEquality(configManager.getEnvironmentConfig(),config); }

	 public synchronized void setMutableConfig( EnvironmentMutableConfig config) throws DatabaseException { EnvironmentConfig newConfig=DbInternal.cloneConfig(configManager.getEnvironmentConfig()); DbInternal.copyMutablePropsTo(config,newConfig); configManager=new DbConfigManager(newConfig); for (int i=configObservers.size() - 1; i >= 0; i-=1) { EnvConfigObserver o=(EnvConfigObserver)configObservers.get(i); o.envConfigUpdate(configManager); } }

	 public synchronized void addConfigObserver( EnvConfigObserver o){ configObservers.add(o); }

	 public synchronized void removeConfigObserver( EnvConfigObserver o){ configObservers.remove(o); }

	 public INList getInMemoryINs(){ return inMemoryINs; }

	 public TxnManager getTxnManager(){ return txnManager; }

	 public Checkpointer getCheckpointer(){ return checkpointer; }

	 public Cleaner getCleaner(){ return cleaner; }

	 public MemoryBudget getMemoryBudget(){ return memoryBudget; }

	 public RecoveryInfo getLastRecoveryInfo(){ return lastRecoveryInfo; }

	 public File getEnvironmentHome(){ return envHome; }

	 public long getTxnTimeout(){ return txnTimeout; }

	 public long getLockTimeout(){ return lockTimeout; }

	 public static boolean maybeForceYield(){ if (forcedYield) { Thread.yield(); } return true; }

