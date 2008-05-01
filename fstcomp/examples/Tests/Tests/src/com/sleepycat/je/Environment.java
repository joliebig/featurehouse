package com.sleepycat.je; 
import java.io.File; 
import java.io.FileInputStream; 
import java.io.FileNotFoundException; 
import java.io.IOException; 
import java.io.PrintStream; 
import java.util.Collections; 
import java.util.HashSet; 
import java.util.Iterator; 
import java.util.List; 
import java.util.Map; 
import java.util.Properties; 
import java.util.Set; 
import java.util.logging.Level; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.dbi.DbEnvPool; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.txn.Locker; 
import com.sleepycat.je.txn.LockerFactory; 
import com.sleepycat.je.utilint.Tracer; 
import de.ovgu.cide.jakutil.*; 
public  class  Environment {
	 private static final String PROPFILE_NAME="je.properties";

	 protected EnvironmentImpl environmentImpl;

	 private TransactionConfig defaultTxnConfig;

	 private EnvironmentMutableConfig handleConfig;

	 private Set referringDbs;

	 private Set referringDbTxns;

	 private boolean valid;

	 public Environment( File envHome, EnvironmentConfig configuration) throws DatabaseException { environmentImpl=null; referringDbs=Collections.synchronizedSet(new HashSet()); referringDbTxns=Collections.synchronizedSet(new HashSet()); valid=false; DatabaseUtil.checkForNullParam(envHome,"envHome"); EnvironmentConfig baseConfig=(configuration == null) ? EnvironmentConfig.DEFAULT : configuration; EnvironmentConfig useConfig=baseConfig.cloneConfig(); applyFileConfig(envHome,useConfig); copyToHandleConfig(useConfig,useConfig); DbEnvPool.EnvironmentImplInfo envInfo=DbEnvPool.getInstance().getEnvironment(envHome,useConfig); environmentImpl=envInfo.envImpl; environmentImpl.checkIfInvalid(); if (!envInfo.firstHandle && configuration != null) {
synchronized (environmentImpl) { environmentImpl.checkImmutablePropsForEquality(useConfig); } } if (!valid) { valid=true; } environmentImpl.incReferenceCount(); }

	 Environment( File envHome) throws DatabaseException { environmentImpl=null; valid=false; DbEnvPool.EnvironmentImplInfo envInfo=DbEnvPool.getInstance().getExistingEnvironment(envHome); EnvironmentImpl foundImpl=envInfo.envImpl; if (foundImpl != null) { foundImpl.checkIfInvalid(); environmentImpl=foundImpl; environmentImpl.incReferenceCount(); EnvironmentConfig useConfig=EnvironmentConfig.DEFAULT.cloneConfig(); applyFileConfig(envHome,useConfig); copyToHandleConfig(useConfig,useConfig); referringDbs=Collections.synchronizedSet(new HashSet()); valid=true; } }

	 private void applyFileConfig( File envHome, EnvironmentMutableConfig useConfig) throws IllegalArgumentException { if (useConfig.getLoadPropertyFile()) { File paramFile=null; try { paramFile=new File(envHome,PROPFILE_NAME); Properties fileProps=new Properties(); FileInputStream fis=new FileInputStream(paramFile); fileProps.load(fis); fis.close(); useConfig.validateProperties(fileProps); Iterator iter=fileProps.entrySet().iterator(); while (iter.hasNext()) { Map.Entry propPair=(Map.Entry)iter.next(); String name=(String)propPair.getKey(); String value=(String)propPair.getValue(); useConfig.setConfigParam(name,value); } } catch ( FileNotFoundException e) { }
catch ( IOException e) { IllegalArgumentException e2=new IllegalArgumentException("An error occurred when reading " + paramFile); e2.initCause(e); throw e2; } } }

	 public synchronized void close() throws DatabaseException { checkHandleIsValid(); try { checkEnv(); } catch ( RunRecoveryException e) { if (environmentImpl != null) { environmentImpl.closeAfterRunRecovery(); } return; } StringBuffer errors=new StringBuffer(); try { if (referringDbs != null) { int nDbs=referringDbs.size(); if (nDbs != 0) { errors.append("There "); if (nDbs == 1) { errors.append("is 1 open Database in the Environment.\n"); } else { errors.append("are "); errors.append(nDbs); errors.append(" open Database in the Environment.\n"); } errors.append("Closing the following databases:\n"); Iterator iter=referringDbs.iterator(); while (iter.hasNext()) { Database db=(Database)iter.next(); String dbName=db.getDebugName(); errors.append(dbName).append(" "); try { db.close(); } catch ( RunRecoveryException e) { throw e; }
catch ( DatabaseException DBE) { errors.append("\nWhile closing Database "); errors.append(dbName); errors.append(" encountered exception: "); errors.append(DBE).append("\n"); } } } } if (referringDbTxns != null) { int nTxns=referringDbTxns.size(); if (nTxns != 0) { Iterator iter=referringDbTxns.iterator(); errors.append("There "); if (nTxns == 1) { errors.append("is 1 existing transaction opened against"); errors.append(" the Environment.\n"); } else { errors.append("are "); errors.append(nTxns); errors.append(" existing transactions opened against"); errors.append(" the Environment.\n"); } errors.append("Aborting open transactions ...\n"); while (iter.hasNext()) { Transaction txn=(Transaction)iter.next(); try { txn.abort(); } catch ( RunRecoveryException e) { throw e; }
catch ( DatabaseException DBE) { errors.append("\nWhile aborting transaction "); errors.append(txn.getId()); errors.append(" encountered exception: "); errors.append(DBE).append("\n"); } } } } try { environmentImpl.close(); } catch ( RunRecoveryException e) { throw e; }
catch ( DatabaseException DBE) { errors.append("\nWhile closing Environment encountered exception: "); errors.append(DBE).append("\n"); } } finally { environmentImpl=null; valid=false; if (errors.length() > 0) { throw new DatabaseException(errors.toString()); } } }

	 public synchronized Database openDatabase( Transaction txn, String databaseName, DatabaseConfig dbConfig) throws DatabaseException { if (dbConfig == null) { dbConfig=DatabaseConfig.DEFAULT; } Database db=new Database(this); openDb(txn,db,databaseName,dbConfig,false); return db; }

	 public synchronized SecondaryDatabase openSecondaryDatabase( Transaction txn, String databaseName, Database primaryDatabase, SecondaryConfig dbConfig) throws DatabaseException { if (dbConfig == null) { dbConfig=SecondaryConfig.DEFAULT; } SecondaryDatabase db=new SecondaryDatabase(this,dbConfig,primaryDatabase); openDb(txn,db,databaseName,dbConfig,dbConfig.getAllowPopulate()); return db; }

	 private void openDb( Transaction txn, Database newDb, String databaseName, DatabaseConfig dbConfig, boolean needWritableLockerForInit) throws DatabaseException { checkEnv(); DatabaseUtil.checkForNullParam(databaseName,"databaseName"); this.hook58(databaseName,dbConfig); validateDbConfigAgainstEnv(dbConfig,databaseName); Locker locker=null; boolean operationOk=false; boolean dbIsClosing=false; try { boolean isWritableLocker=true; if (needWritableLockerForInit) { locker=LockerFactory.getWritableLocker(this,txn,dbConfig.getTransactional(),true,null); isWritableLocker=true; } else { locker=LockerFactory.getReadableLocker(this,txn,dbConfig.getTransactional(),true,false); isWritableLocker=!dbConfig.getTransactional() || locker.isTransactional(); } DatabaseImpl database=environmentImpl.getDb(locker,databaseName,newDb); boolean databaseExists=false; databaseExists=this.hook59(database,databaseExists); if (databaseExists) { if (dbConfig.getAllowCreate() && dbConfig.getExclusiveCreate()) { dbIsClosing=true; throw new DatabaseException("Database " + databaseName + " already exists"); } newDb.initExisting(this,locker,database,dbConfig); } else { if (dbConfig.getAllowCreate()) { if (!isWritableLocker) { locker.operationEnd(OperationStatus.SUCCESS); locker=LockerFactory.getWritableLocker(this,txn,dbConfig.getTransactional(),true,null); isWritableLocker=true; } newDb.initNew(this,locker,databaseName,dbConfig); } else { throw new DatabaseNotFoundException("Database " + databaseName + " not found."); } } operationOk=true; addReferringHandle(newDb); } finally { if (locker != null) { locker.setHandleLockOwner(operationOk,newDb,dbIsClosing); locker.operationEnd(operationOk); } } }

	 private void validateDbConfigAgainstEnv( DatabaseConfig dbConfig, String databaseName) throws DatabaseException { if (dbConfig.getTransactional() && !(environmentImpl.isTransactional())) { throw new DatabaseException("Attempted to open Database " + databaseName + " transactionally, but parent Environment is"+ " not transactional"); } if (environmentImpl.isReadOnly() && (!dbConfig.getReadOnly())) { throw new DatabaseException("Attempted to open Database " + databaseName + " as writable but parent Environment is read only "); } }

	 public File getHome() throws DatabaseException { checkHandleIsValid(); return environmentImpl.getEnvironmentHome(); }

	 TransactionConfig getDefaultTxnConfig(){ return defaultTxnConfig; }

	 private void copyToHandleConfig( EnvironmentMutableConfig useConfig, EnvironmentConfig initStaticConfig) throws DatabaseException { EnvironmentMutableConfig newHandleConfig=new EnvironmentMutableConfig(); useConfig.copyHandlePropsTo(newHandleConfig); this.handleConfig=newHandleConfig; TransactionConfig newTxnConfig=TransactionConfig.DEFAULT.cloneConfig(); newTxnConfig.setNoSync(handleConfig.getTxnNoSync()); newTxnConfig.setWriteNoSync(handleConfig.getTxnWriteNoSync()); if (initStaticConfig != null) { newTxnConfig.setSerializableIsolation(initStaticConfig.getTxnSerializableIsolation()); newTxnConfig.setReadCommitted(initStaticConfig.getTxnReadCommitted()); } else { newTxnConfig.setSerializableIsolation(defaultTxnConfig.getSerializableIsolation()); newTxnConfig.setReadCommitted(defaultTxnConfig.getReadCommitted()); } this.defaultTxnConfig=newTxnConfig; }

	 public Transaction beginTransaction( Transaction parent, TransactionConfig txnConfig) throws DatabaseException { checkHandleIsValid(); checkEnv(); if (!environmentImpl.isTransactional()) { throw new DatabaseException("Transactions can not be used in a non-transactional " + "environment"); } if (txnConfig != null && ((txnConfig.getSerializableIsolation() && txnConfig.getReadUncommitted()) || (txnConfig.getSerializableIsolation() && txnConfig.getReadCommitted()) || (txnConfig.getReadUncommitted() && txnConfig.getReadCommitted()))) { throw new IllegalArgumentException("Only one may be specified: SerializableIsolation, " + "ReadCommitted or ReadUncommitted"); } TransactionConfig useConfig=null; if (txnConfig == null) { useConfig=defaultTxnConfig; } else { if (defaultTxnConfig.getNoSync() || defaultTxnConfig.getWriteNoSync()) { if (!txnConfig.getNoSync() && !txnConfig.getSync() && !txnConfig.getWriteNoSync()) { useConfig=txnConfig.cloneConfig(); if (defaultTxnConfig.getWriteNoSync()) { useConfig.setWriteNoSync(true); } else { useConfig.setNoSync(true); } } } if (!txnConfig.getSerializableIsolation() && !txnConfig.getReadCommitted() && !txnConfig.getReadUncommitted()) { if (defaultTxnConfig.getSerializableIsolation()) { if (useConfig == null) { useConfig=txnConfig.cloneConfig(); } useConfig.setSerializableIsolation(true); } else if (defaultTxnConfig.getReadCommitted()) { if (useConfig == null) { useConfig=txnConfig.cloneConfig(); } useConfig.setReadCommitted(true); } } if (useConfig == null) { useConfig=txnConfig; } } Transaction txn=new Transaction(this,environmentImpl.txnBegin(parent,useConfig)); addReferringHandle(txn); return txn; }

	 public void checkpoint( CheckpointConfig ckptConfig) throws DatabaseException { checkHandleIsValid(); checkEnv(); CheckpointConfig useConfig=(ckptConfig == null) ? CheckpointConfig.DEFAULT : ckptConfig; environmentImpl.invokeCheckpoint(useConfig,false,"api"); }

	 public void sync() throws DatabaseException { checkHandleIsValid(); checkEnv(); CheckpointConfig config=new CheckpointConfig(); config.setForce(true); environmentImpl.invokeCheckpoint(config,true,"sync"); }

	 public int cleanLog() throws DatabaseException { checkHandleIsValid(); checkEnv(); return environmentImpl.invokeCleaner(); }

	 public EnvironmentConfig getConfig() throws DatabaseException { checkHandleIsValid(); EnvironmentConfig config=environmentImpl.cloneConfig(); handleConfig.copyHandlePropsTo(config); config.fillInEnvironmentGeneratedProps(environmentImpl); return config; }

	 public void setMutableConfig( EnvironmentMutableConfig mutableConfig) throws DatabaseException { checkHandleIsValid(); DatabaseUtil.checkForNullParam(mutableConfig,"mutableConfig"); environmentImpl.setMutableConfig(mutableConfig); copyToHandleConfig(mutableConfig,null); }

	 public EnvironmentMutableConfig getMutableConfig() throws DatabaseException { checkHandleIsValid(); EnvironmentMutableConfig config=environmentImpl.cloneMutableConfig(); handleConfig.copyHandlePropsTo(config); return config; }

	 void upgrade() throws DatabaseException { }

	 public List getDatabaseNames() throws DatabaseException { checkHandleIsValid(); checkEnv(); return environmentImpl.getDbNames(); }

	 public Transaction getThreadTransaction() throws DatabaseException { return (Transaction)environmentImpl.getTxnManager().getTxnForThread(); }

	 public void setThreadTransaction( Transaction txn){ environmentImpl.getTxnManager().setTxnForThread(txn); }

	 void addReferringHandle( Database db){ referringDbs.add(db); }

	 void addReferringHandle( Transaction txn){ referringDbTxns.add(txn); }

	 void removeReferringHandle( Database db){ referringDbs.remove(db); }

	 void removeReferringHandle( Transaction txn){ referringDbTxns.remove(txn); }

	 EnvironmentImpl getEnvironmentImpl(){ return environmentImpl; }

	 protected void checkHandleIsValid() throws DatabaseException { if (!valid) { throw new DatabaseException("Attempt to use non-open Environment object()."); } }

	 protected void checkEnv() throws DatabaseException, RunRecoveryException { if (environmentImpl == null) { return; } environmentImpl.checkIfInvalid(); environmentImpl.checkNotClosed(); }

	 protected void hook58( String databaseName, DatabaseConfig dbConfig) throws DatabaseException { }

	 protected boolean hook59( DatabaseImpl database, boolean databaseExists) throws DatabaseException { return databaseExists; }


}
