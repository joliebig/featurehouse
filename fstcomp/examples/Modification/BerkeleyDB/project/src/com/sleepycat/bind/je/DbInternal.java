package com.sleepycat.je; 
import java.io.File; 
import com.sleepycat.je.dbi.CursorImpl; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.dbi.GetMode; 
import com.sleepycat.je.txn.Locker; 
import de.ovgu.cide.jakutil.*; 
public  class  DbInternal {
	 public static void dbInvalidate__wrappee__base( Database db){ db.invalidate(); }

	 public static void dbInvalidate( Database db){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dbInvalidate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void dbSetHandleLocker__wrappee__base( Database db, Locker locker){ db.setHandleLocker(locker); }

	 public static void dbSetHandleLocker( Database db, Locker locker){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dbSetHandleLocker__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static EnvironmentImpl envGetEnvironmentImpl__wrappee__base( Environment env){ return env.getEnvironmentImpl(); }

	 public static EnvironmentImpl envGetEnvironmentImpl( Environment env){ t.in(Thread.currentThread().getStackTrace()[1].toString());	envGetEnvironmentImpl__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static Cursor newCursor__wrappee__base( DatabaseImpl dbImpl, Locker locker, CursorConfig cursorConfig) throws DatabaseException { return new Cursor(dbImpl,locker,cursorConfig); }

	 public static Cursor newCursor( DatabaseImpl dbImpl, Locker locker, CursorConfig cursorConfig) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	newCursor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static OperationStatus position__wrappee__base( Cursor cursor, DatabaseEntry key, DatabaseEntry data, LockMode lockMode, boolean first) throws DatabaseException { return cursor.position(key,data,lockMode,first); }

	 public static OperationStatus position( Cursor cursor, DatabaseEntry key, DatabaseEntry data, LockMode lockMode, boolean first) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	position__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static OperationStatus retrieveNext__wrappee__base( Cursor cursor, DatabaseEntry key, DatabaseEntry data, LockMode lockMode, GetMode getMode) throws DatabaseException { return cursor.retrieveNext(key,data,lockMode,getMode); }

	 public static OperationStatus retrieveNext( Cursor cursor, DatabaseEntry key, DatabaseEntry data, LockMode lockMode, GetMode getMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	retrieveNext__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static boolean advanceCursor__wrappee__base( Cursor cursor, DatabaseEntry key, DatabaseEntry data){ return cursor.advanceCursor(key,data); }

	 public static boolean advanceCursor( Cursor cursor, DatabaseEntry key, DatabaseEntry data){ t.in(Thread.currentThread().getStackTrace()[1].toString());	advanceCursor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static CursorImpl getCursorImpl__wrappee__base( Cursor cursor){ return cursor.getCursorImpl(); }

	 public static CursorImpl getCursorImpl( Cursor cursor){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getCursorImpl__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static DatabaseImpl dbGetDatabaseImpl__wrappee__base( Database db){ return db.getDatabaseImpl(); }

	 public static DatabaseImpl dbGetDatabaseImpl( Database db){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dbGetDatabaseImpl__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static Cursor\[\] getSortedCursors__wrappee__base( JoinCursor cursor){ return cursor.getSortedCursors(); }

	 public static Cursor[] getSortedCursors( JoinCursor cursor){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getSortedCursors__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void setLoadPropertyFile__wrappee__base( EnvironmentConfig config, boolean loadProperties){ config.setLoadPropertyFile(loadProperties); }

	 public static void setLoadPropertyFile( EnvironmentConfig config, boolean loadProperties){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setLoadPropertyFile__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void setCreateUP__wrappee__base( EnvironmentConfig config, boolean checkpointUP){ config.setCreateUP(checkpointUP); }

	 public static void setCreateUP( EnvironmentConfig config, boolean checkpointUP){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setCreateUP__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static boolean getCreateUP__wrappee__base( EnvironmentConfig config){ return config.getCreateUP(); }

	 public static boolean getCreateUP( EnvironmentConfig config){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getCreateUP__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void setCheckpointUP__wrappee__base( EnvironmentConfig config, boolean checkpointUP){ config.setCheckpointUP(checkpointUP); }

	 public static void setCheckpointUP( EnvironmentConfig config, boolean checkpointUP){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setCheckpointUP__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static boolean getCheckpointUP__wrappee__base( EnvironmentConfig config){ return config.getCheckpointUP(); }

	 public static boolean getCheckpointUP( EnvironmentConfig config){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getCheckpointUP__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void setTxnReadCommitted__wrappee__base( EnvironmentConfig config, boolean txnReadCommitted){ config.setTxnReadCommitted(txnReadCommitted); }

	 public static void setTxnReadCommitted( EnvironmentConfig config, boolean txnReadCommitted){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setTxnReadCommitted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static boolean getTxnReadCommitted__wrappee__base( EnvironmentConfig config){ return config.getTxnReadCommitted(); }

	 public static boolean getTxnReadCommitted( EnvironmentConfig config){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTxnReadCommitted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static EnvironmentConfig cloneConfig__wrappee__base( EnvironmentConfig config){ return config.cloneConfig(); }

	 public static EnvironmentConfig cloneConfig( EnvironmentConfig config){ t.in(Thread.currentThread().getStackTrace()[1].toString());	cloneConfig__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static EnvironmentMutableConfig cloneMutableConfig__wrappee__base( EnvironmentMutableConfig config){ return config.cloneMutableConfig(); }

	 public static EnvironmentMutableConfig cloneMutableConfig( EnvironmentMutableConfig config){ t.in(Thread.currentThread().getStackTrace()[1].toString());	cloneMutableConfig__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void checkImmutablePropsForEquality__wrappee__base( EnvironmentMutableConfig config, EnvironmentMutableConfig passedConfig) throws IllegalArgumentException { config.checkImmutablePropsForEquality(passedConfig); }

	 public static void checkImmutablePropsForEquality( EnvironmentMutableConfig config, EnvironmentMutableConfig passedConfig) throws IllegalArgumentException { t.in(Thread.currentThread().getStackTrace()[1].toString());	checkImmutablePropsForEquality__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void copyMutablePropsTo__wrappee__base( EnvironmentMutableConfig config, EnvironmentMutableConfig toConfig){ config.copyMutablePropsTo(toConfig); }

	 public static void copyMutablePropsTo( EnvironmentMutableConfig config, EnvironmentMutableConfig toConfig){ t.in(Thread.currentThread().getStackTrace()[1].toString());	copyMutablePropsTo__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void disableParameterValidation__wrappee__base( EnvironmentMutableConfig config){ config.setValidateParams(false); }

	 public static void disableParameterValidation( EnvironmentMutableConfig config){ t.in(Thread.currentThread().getStackTrace()[1].toString());	disableParameterValidation__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void setUseExistingConfig__wrappee__base( DatabaseConfig config, boolean useExistingConfig){ config.setUseExistingConfig(useExistingConfig); }

	 public static void setUseExistingConfig( DatabaseConfig config, boolean useExistingConfig){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setUseExistingConfig__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void databaseConfigValidate__wrappee__base( DatabaseConfig config1, DatabaseConfig config2) throws DatabaseException { config1.validate(config2); }

	 public static void databaseConfigValidate( DatabaseConfig config1, DatabaseConfig config2) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	databaseConfigValidate__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static Locker getLocker__wrappee__base( Transaction txn) throws DatabaseException { return txn.getLocker(); }

	 public static Locker getLocker( Transaction txn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getLocker__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static TransactionConfig getDefaultTxnConfig__wrappee__base( Environment env){ return env.getDefaultTxnConfig(); }

	 public static TransactionConfig getDefaultTxnConfig( Environment env){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getDefaultTxnConfig__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static Environment getEnvironmentShell__wrappee__base( File environmentHome){ Environment env=null; try { env=new Environment(environmentHome); if (env.getEnvironmentImpl() == null) { env=null; } } catch ( DatabaseException e) { } return env; }

	 public static Environment getEnvironmentShell( File environmentHome){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getEnvironmentShell__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
