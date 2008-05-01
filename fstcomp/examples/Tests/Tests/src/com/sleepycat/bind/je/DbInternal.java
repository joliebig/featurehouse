package com.sleepycat.je; 
import java.io.File; 
import com.sleepycat.je.dbi.CursorImpl; 
import com.sleepycat.je.dbi.DatabaseImpl; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import com.sleepycat.je.dbi.GetMode; 
import com.sleepycat.je.txn.Locker; 
import de.ovgu.cide.jakutil.*; 
public  class  DbInternal {
	 public static void dbInvalidate( Database db){ db.invalidate(); }

	 public static void dbSetHandleLocker( Database db, Locker locker){ db.setHandleLocker(locker); }

	 public static EnvironmentImpl envGetEnvironmentImpl( Environment env){ return env.getEnvironmentImpl(); }

	 public static Cursor newCursor( DatabaseImpl dbImpl, Locker locker, CursorConfig cursorConfig) throws DatabaseException { return new Cursor(dbImpl,locker,cursorConfig); }

	 public static OperationStatus position( Cursor cursor, DatabaseEntry key, DatabaseEntry data, LockMode lockMode, boolean first) throws DatabaseException { return cursor.position(key,data,lockMode,first); }

	 public static OperationStatus retrieveNext( Cursor cursor, DatabaseEntry key, DatabaseEntry data, LockMode lockMode, GetMode getMode) throws DatabaseException { return cursor.retrieveNext(key,data,lockMode,getMode); }

	 public static boolean advanceCursor( Cursor cursor, DatabaseEntry key, DatabaseEntry data){ return cursor.advanceCursor(key,data); }

	 public static CursorImpl getCursorImpl( Cursor cursor){ return cursor.getCursorImpl(); }

	 public static DatabaseImpl dbGetDatabaseImpl( Database db){ return db.getDatabaseImpl(); }

	 public static Cursor[] getSortedCursors( JoinCursor cursor){ return cursor.getSortedCursors(); }

	 public static void setLoadPropertyFile( EnvironmentConfig config, boolean loadProperties){ config.setLoadPropertyFile(loadProperties); }

	 public static void setCreateUP( EnvironmentConfig config, boolean checkpointUP){ config.setCreateUP(checkpointUP); }

	 public static boolean getCreateUP( EnvironmentConfig config){ return config.getCreateUP(); }

	 public static void setCheckpointUP( EnvironmentConfig config, boolean checkpointUP){ config.setCheckpointUP(checkpointUP); }

	 public static boolean getCheckpointUP( EnvironmentConfig config){ return config.getCheckpointUP(); }

	 public static void setTxnReadCommitted( EnvironmentConfig config, boolean txnReadCommitted){ config.setTxnReadCommitted(txnReadCommitted); }

	 public static boolean getTxnReadCommitted( EnvironmentConfig config){ return config.getTxnReadCommitted(); }

	 public static EnvironmentConfig cloneConfig( EnvironmentConfig config){ return config.cloneConfig(); }

	 public static EnvironmentMutableConfig cloneMutableConfig( EnvironmentMutableConfig config){ return config.cloneMutableConfig(); }

	 public static void checkImmutablePropsForEquality( EnvironmentMutableConfig config, EnvironmentMutableConfig passedConfig) throws IllegalArgumentException { config.checkImmutablePropsForEquality(passedConfig); }

	 public static void copyMutablePropsTo( EnvironmentMutableConfig config, EnvironmentMutableConfig toConfig){ config.copyMutablePropsTo(toConfig); }

	 public static void disableParameterValidation( EnvironmentMutableConfig config){ config.setValidateParams(false); }

	 public static void setUseExistingConfig( DatabaseConfig config, boolean useExistingConfig){ config.setUseExistingConfig(useExistingConfig); }

	 public static void databaseConfigValidate( DatabaseConfig config1, DatabaseConfig config2) throws DatabaseException { config1.validate(config2); }

	 public static Locker getLocker( Transaction txn) throws DatabaseException { return txn.getLocker(); }

	 public static TransactionConfig getDefaultTxnConfig( Environment env){ return env.getDefaultTxnConfig(); }

	 public static Environment getEnvironmentShell( File environmentHome){ Environment env=null; try { env=new Environment(environmentHome); if (env.getEnvironmentImpl() == null) { env=null; } } catch ( DatabaseException e) { } return env; }


}
