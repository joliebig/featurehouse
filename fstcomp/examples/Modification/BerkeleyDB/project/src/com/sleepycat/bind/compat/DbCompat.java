package com.sleepycat.compat; 
import java.io.*; 
import java.util.Comparator; 
import com.sleepycat.je.Cursor; 
import com.sleepycat.je.CursorConfig; 
import com.sleepycat.je.Database; 
import com.sleepycat.je.DatabaseConfig; 
import com.sleepycat.je.DatabaseEntry; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.Environment; 
import com.sleepycat.je.EnvironmentConfig; 
import com.sleepycat.je.LockMode; 
import com.sleepycat.je.OperationStatus; 
import com.sleepycat.je.SecondaryConfig; 
import com.sleepycat.je.SecondaryCursor; 
import com.sleepycat.je.SecondaryDatabase; 
import com.sleepycat.je.Transaction; 
import com.sleepycat.je.TransactionConfig; 
import de.ovgu.cide.jakutil.*; 
public  class  DbCompat {
	 public static final boolean CDB=false;

	 public static final boolean JOIN=true;

	 public static final boolean NESTED_TRANSACTIONS=false;

	 public static final boolean INSERTION_ORDERED_DUPLICATES=false;

	 public static final boolean SEPARATE_DATABASE_FILES=false;

	 public static final boolean MEMORY_SUBSYSTEM=false;

	 public static final boolean LOCK_SUBSYSTEM=false;

	 public static final boolean HASH_METHOD=false;

	 public static final boolean RECNO_METHOD=false;

	 public static final boolean QUEUE_METHOD=false;

	 public static final boolean BTREE_RECNUM_METHOD=false;

	 public static final boolean OPTIONAL_READ_UNCOMMITTED=false;

	 public static final boolean SECONDARIES=true;

	 public static boolean getInitializeLocking__wrappee__base( EnvironmentConfig config){ return true; }

	 public static boolean getInitializeLocking( EnvironmentConfig config){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getInitializeLocking__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static boolean getInitializeCDB__wrappee__base( EnvironmentConfig config){ return false; }

	 public static boolean getInitializeCDB( EnvironmentConfig config){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getInitializeCDB__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static boolean isTypeBtree__wrappee__base( DatabaseConfig dbConfig){ return true; }

	 public static boolean isTypeBtree( DatabaseConfig dbConfig){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isTypeBtree__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static boolean isTypeHash__wrappee__base( DatabaseConfig dbConfig){ return false; }

	 public static boolean isTypeHash( DatabaseConfig dbConfig){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isTypeHash__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static boolean isTypeQueue__wrappee__base( DatabaseConfig dbConfig){ return false; }

	 public static boolean isTypeQueue( DatabaseConfig dbConfig){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isTypeQueue__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static boolean isTypeRecno__wrappee__base( DatabaseConfig dbConfig){ return false; }

	 public static boolean isTypeRecno( DatabaseConfig dbConfig){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isTypeRecno__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static boolean getBtreeRecordNumbers__wrappee__base( DatabaseConfig dbConfig){ return false; }

	 public static boolean getBtreeRecordNumbers( DatabaseConfig dbConfig){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBtreeRecordNumbers__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static boolean getReadUncommitted__wrappee__base( DatabaseConfig dbConfig){ return true; }

	 public static boolean getReadUncommitted( DatabaseConfig dbConfig){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getReadUncommitted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static boolean getRenumbering__wrappee__base( DatabaseConfig dbConfig){ return false; }

	 public static boolean getRenumbering( DatabaseConfig dbConfig){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getRenumbering__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static boolean getSortedDuplicates__wrappee__base( DatabaseConfig dbConfig){ return dbConfig.getSortedDuplicates(); }

	 public static boolean getSortedDuplicates( DatabaseConfig dbConfig){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getSortedDuplicates__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static boolean getUnsortedDuplicates__wrappee__base( DatabaseConfig dbConfig){ return false; }

	 public static boolean getUnsortedDuplicates( DatabaseConfig dbConfig){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getUnsortedDuplicates__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static CursorConfig cloneCursorConfig__wrappee__base( CursorConfig config){ CursorConfig newConfig=new CursorConfig(); newConfig.setReadCommitted(config.getReadCommitted()); newConfig.setReadUncommitted(config.getReadUncommitted()); return newConfig; }

	 public static CursorConfig cloneCursorConfig( CursorConfig config){ t.in(Thread.currentThread().getStackTrace()[1].toString());	cloneCursorConfig__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static boolean getWriteCursor__wrappee__base( CursorConfig config){ return false; }

	 public static boolean getWriteCursor( CursorConfig config){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getWriteCursor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void setWriteCursor__wrappee__base( CursorConfig config, boolean write){ if (write) { throw new UnsupportedOperationException(); } }

	 public static void setWriteCursor( CursorConfig config, boolean write){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setWriteCursor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void setRecordNumber__wrappee__base( DatabaseEntry entry, int recNum){ throw new UnsupportedOperationException(); }

	 public static void setRecordNumber( DatabaseEntry entry, int recNum){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setRecordNumber__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static int getRecordNumber__wrappee__base( DatabaseEntry entry){ throw new UnsupportedOperationException(); }

	 public static int getRecordNumber( DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getRecordNumber__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static String getDatabaseFile__wrappee__base( Database db) throws DatabaseException { return null; }

	 public static String getDatabaseFile( Database db) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getDatabaseFile__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static OperationStatus getCurrentRecordNumber__wrappee__base( Cursor cursor, DatabaseEntry key, LockMode lockMode) throws DatabaseException { throw new UnsupportedOperationException(); }

	 public static OperationStatus getCurrentRecordNumber( Cursor cursor, DatabaseEntry key, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getCurrentRecordNumber__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static OperationStatus getSearchRecordNumber__wrappee__base( Cursor cursor, DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { throw new UnsupportedOperationException(); }

	 public static OperationStatus getSearchRecordNumber( Cursor cursor, DatabaseEntry key, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getSearchRecordNumber__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static OperationStatus getSearchRecordNumber__wrappee__base( SecondaryCursor cursor, DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { throw new UnsupportedOperationException(); }

	 public static OperationStatus getSearchRecordNumber( SecondaryCursor cursor, DatabaseEntry key, DatabaseEntry pKey, DatabaseEntry data, LockMode lockMode) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getSearchRecordNumber__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static OperationStatus putAfter__wrappee__base( Cursor cursor, DatabaseEntry key, DatabaseEntry data) throws DatabaseException { throw new UnsupportedOperationException(); }

	 public static OperationStatus putAfter( Cursor cursor, DatabaseEntry key, DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	putAfter__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static OperationStatus putBefore__wrappee__base( Cursor cursor, DatabaseEntry key, DatabaseEntry data) throws DatabaseException { throw new UnsupportedOperationException(); }

	 public static OperationStatus putBefore( Cursor cursor, DatabaseEntry key, DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	putBefore__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static OperationStatus append__wrappee__base( Database db, Transaction txn, DatabaseEntry key, DatabaseEntry data) throws DatabaseException { throw new UnsupportedOperationException(); }

	 public static OperationStatus append( Database db, Transaction txn, DatabaseEntry key, DatabaseEntry data) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	append__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static Transaction getThreadTransaction__wrappee__base( Environment env) throws DatabaseException { return env.getThreadTransaction(); }

	 public static Transaction getThreadTransaction( Environment env) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getThreadTransaction__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void setInitializeCache__wrappee__base( EnvironmentConfig config, boolean val){ if (!val) { throw new UnsupportedOperationException(); } }

	 public static void setInitializeCache( EnvironmentConfig config, boolean val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setInitializeCache__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void setInitializeLocking__wrappee__base( EnvironmentConfig config, boolean val){ if (!val) { throw new UnsupportedOperationException(); } }

	 public static void setInitializeLocking( EnvironmentConfig config, boolean val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setInitializeLocking__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void setInitializeCDB__wrappee__base( EnvironmentConfig config, boolean val){ if (val) { throw new UnsupportedOperationException(); } }

	 public static void setInitializeCDB( EnvironmentConfig config, boolean val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setInitializeCDB__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void setLockDetectModeOldest__wrappee__base( EnvironmentConfig config){ }

	 public static void setLockDetectModeOldest( EnvironmentConfig config){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setLockDetectModeOldest__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void setSerializableIsolation__wrappee__base( TransactionConfig config, boolean val){ config.setSerializableIsolation(val); }

	 public static void setSerializableIsolation( TransactionConfig config, boolean val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setSerializableIsolation__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void setBtreeComparator__wrappee__base( DatabaseConfig dbConfig, Comparator comparator){ dbConfig.setBtreeComparator(comparator.getClass()); }

	 public static void setBtreeComparator( DatabaseConfig dbConfig, Comparator comparator){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setBtreeComparator__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void setTypeBtree__wrappee__base( DatabaseConfig dbConfig){ }

	 public static void setTypeBtree( DatabaseConfig dbConfig){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setTypeBtree__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void setTypeHash__wrappee__base( DatabaseConfig dbConfig){ throw new UnsupportedOperationException(); }

	 public static void setTypeHash( DatabaseConfig dbConfig){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setTypeHash__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void setTypeRecno__wrappee__base( DatabaseConfig dbConfig){ throw new UnsupportedOperationException(); }

	 public static void setTypeRecno( DatabaseConfig dbConfig){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setTypeRecno__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void setTypeQueue__wrappee__base( DatabaseConfig dbConfig){ throw new UnsupportedOperationException(); }

	 public static void setTypeQueue( DatabaseConfig dbConfig){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setTypeQueue__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void setBtreeRecordNumbers__wrappee__base( DatabaseConfig dbConfig, boolean val){ throw new UnsupportedOperationException(); }

	 public static void setBtreeRecordNumbers( DatabaseConfig dbConfig, boolean val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setBtreeRecordNumbers__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void setReadUncommitted__wrappee__base( DatabaseConfig dbConfig, boolean val){ }

	 public static void setReadUncommitted( DatabaseConfig dbConfig, boolean val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setReadUncommitted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void setRenumbering__wrappee__base( DatabaseConfig dbConfig, boolean val){ throw new UnsupportedOperationException(); }

	 public static void setRenumbering( DatabaseConfig dbConfig, boolean val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setRenumbering__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void setSortedDuplicates__wrappee__base( DatabaseConfig dbConfig, boolean val){ dbConfig.setSortedDuplicates(val); }

	 public static void setSortedDuplicates( DatabaseConfig dbConfig, boolean val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setSortedDuplicates__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void setUnsortedDuplicates__wrappee__base( DatabaseConfig dbConfig, boolean val){ if (val) { throw new UnsupportedOperationException(); } }

	 public static void setUnsortedDuplicates( DatabaseConfig dbConfig, boolean val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setUnsortedDuplicates__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void setRecordLength__wrappee__base( DatabaseConfig dbConfig, int val){ if (val != 0) { throw new UnsupportedOperationException(); } }

	 public static void setRecordLength( DatabaseConfig dbConfig, int val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setRecordLength__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void setRecordPad__wrappee__base( DatabaseConfig dbConfig, int val){ throw new UnsupportedOperationException(); }

	 public static void setRecordPad( DatabaseConfig dbConfig, int val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setRecordPad__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static Database openDatabase__wrappee__base( Environment env, Transaction txn, String file, String name, DatabaseConfig config) throws DatabaseException, FileNotFoundException { return env.openDatabase(txn,makeDbName(file,name),config); }

	 public static Database openDatabase( Environment env, Transaction txn, String file, String name, DatabaseConfig config) throws DatabaseException, FileNotFoundException { t.in(Thread.currentThread().getStackTrace()[1].toString());	openDatabase__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static SecondaryDatabase openSecondaryDatabase__wrappee__base( Environment env, Transaction txn, String file, String name, Database primary, SecondaryConfig config) throws DatabaseException, FileNotFoundException { return env.openSecondaryDatabase(txn,makeDbName(file,name),primary,config); }

	 public static SecondaryDatabase openSecondaryDatabase( Environment env, Transaction txn, String file, String name, Database primary, SecondaryConfig config) throws DatabaseException, FileNotFoundException { t.in(Thread.currentThread().getStackTrace()[1].toString());	openSecondaryDatabase__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private static String makeDbName__wrappee__base( String file, String name){ if (file == null) { return name; } else { if (name != null) { return file + '.' + name; } else { return file; } } }

	 private static String makeDbName( String file, String name){ t.in(Thread.currentThread().getStackTrace()[1].toString());	makeDbName__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
