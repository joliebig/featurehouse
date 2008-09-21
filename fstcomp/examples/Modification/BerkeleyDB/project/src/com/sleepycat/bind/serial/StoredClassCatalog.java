package com.sleepycat.bind.serial; 
import java.io.ByteArrayInputStream; 
import java.io.ByteArrayOutputStream; 
import java.io.IOException; 
import java.io.ObjectInputStream; 
import java.io.ObjectOutputStream; 
import java.io.ObjectStreamClass; 
import java.io.Serializable; 
import java.math.BigInteger; 
import java.util.HashMap; 
import com.sleepycat.compat.DbCompat; 
import com.sleepycat.je.Cursor; 
import com.sleepycat.je.CursorConfig; 
import com.sleepycat.je.Database; 
import com.sleepycat.je.DatabaseConfig; 
import com.sleepycat.je.DatabaseEntry; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.EnvironmentConfig; 
import com.sleepycat.je.LockMode; 
import com.sleepycat.je.OperationStatus; 
import com.sleepycat.util.RuntimeExceptionWrapper; 
import com.sleepycat.util.UtfOps; 
import de.ovgu.cide.jakutil.*; 
public  class  StoredClassCatalog  implements ClassCatalog {
	 private static final byte REC_LAST_CLASS_ID=(byte)0;

	 private static final byte REC_CLASS_FORMAT=(byte)1;

	 private static final byte REC_CLASS_INFO=(byte)2;

	 private static final byte[] LAST_CLASS_ID_KEY={REC_LAST_CLASS_ID};

	 private Database db;

	 private HashMap classMap;

	 private HashMap formatMap;

	 private LockMode writeLockMode;

	 private boolean cdbMode;

	 public StoredClassCatalog( Database database) throws DatabaseException, IllegalArgumentException { db=database; DatabaseConfig dbConfig=db.getConfig(); EnvironmentConfig envConfig=db.getEnvironment().getConfig(); writeLockMode=hook_getLockMode(envConfig); cdbMode=DbCompat.getInitializeCDB(envConfig); if (!DbCompat.isTypeBtree(dbConfig)) { throw new IllegalArgumentException("The class catalog must be a BTREE database."); } if (DbCompat.getSortedDuplicates(dbConfig) || DbCompat.getUnsortedDuplicates(dbConfig)) { throw new IllegalArgumentException("The class catalog database must not allow duplicates."); } classMap=new HashMap(); formatMap=new HashMap(); DatabaseEntry key=new DatabaseEntry(LAST_CLASS_ID_KEY); DatabaseEntry data=new DatabaseEntry(); if (dbConfig.getReadOnly()) { OperationStatus status=db.get(null,key,data,null); if (status != OperationStatus.SUCCESS) { throw new IllegalStateException("A read-only catalog database may not be empty"); } } else { data.setData(new byte[1]); db.putNoOverwrite(null,key,data); } }

	
private static  class  ClassInfo  implements Serializable {
		 private byte[] classID;

		 private transient ObjectStreamClass classFormat;

		 ClassInfo(){ }

		 ClassInfo( DatabaseEntry dbt){ byte[] data=dbt.getData(); int len=data[0]; classID=new byte[len]; System.arraycopy(data,1,classID,0,len); }

		 void toDbt__wrappee__base( DatabaseEntry dbt){ byte[] data=new byte[1 + classID.length]; data[0]=(byte)classID.length; System.arraycopy(classID,0,data,1,classID.length); dbt.setData(data); }

		 void toDbt( DatabaseEntry dbt){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toDbt__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 void setClassID__wrappee__base( byte[] classID){ this.classID=classID; }

		 void setClassID( byte[] classID){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setClassID__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 byte\[\] getClassID__wrappee__base(){ return classID; }

		 byte[] getClassID(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getClassID__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 ObjectStreamClass getClassFormat__wrappee__base(){ return classFormat; }

		 ObjectStreamClass getClassFormat(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getClassFormat__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

		 void setClassFormat__wrappee__base( ObjectStreamClass classFormat){ this.classFormat=classFormat; }

		 void setClassFormat( ObjectStreamClass classFormat){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setClassFormat__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	 private static byte[] ZERO_LENGTH_BYTE_ARRAY=new byte[0];

	 private LockMode hook_getLockMode__wrappee__base( EnvironmentConfig envConfig) throws DatabaseException { return (DbCompat.getInitializeLocking(envConfig)) ? LockMode.RMW : LockMode.DEFAULT; }

	 private LockMode hook_getLockMode( EnvironmentConfig envConfig) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook_getLockMode__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public synchronized void close__wrappee__base() throws DatabaseException { if (db != null) { db.close(); } db=null; formatMap=null; classMap=null; }

	 public synchronized void close() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	close__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public synchronized byte\[\] getClassID__wrappee__base( ObjectStreamClass classFormat) throws DatabaseException, ClassNotFoundException { ClassInfo classInfo=getClassInfo(classFormat); return classInfo.getClassID(); }

	 public synchronized byte[] getClassID( ObjectStreamClass classFormat) throws DatabaseException, ClassNotFoundException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getClassID__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public synchronized ObjectStreamClass getClassFormat__wrappee__base( byte[] classID) throws DatabaseException, ClassNotFoundException { return getClassFormat(classID,new DatabaseEntry()); }

	 public synchronized ObjectStreamClass getClassFormat( byte[] classID) throws DatabaseException, ClassNotFoundException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getClassFormat__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private ObjectStreamClass getClassFormat__wrappee__base( byte[] classID, DatabaseEntry data) throws DatabaseException, ClassNotFoundException { BigInteger classIDObj=new BigInteger(classID); ObjectStreamClass classFormat=(ObjectStreamClass)formatMap.get(classIDObj); if (classFormat == null) { byte[] keyBytes=new byte[classID.length + 1]; keyBytes[0]=REC_CLASS_FORMAT; System.arraycopy(classID,0,keyBytes,1,classID.length); DatabaseEntry key=new DatabaseEntry(keyBytes); OperationStatus status=db.get(null,key,data,LockMode.DEFAULT); if (status != OperationStatus.SUCCESS) { throw new ClassNotFoundException("Catalog class ID not found"); } try { ObjectInputStream ois=new ObjectInputStream(new ByteArrayInputStream(data.getData(),data.getOffset(),data.getSize())); classFormat=(ObjectStreamClass)ois.readObject(); } catch ( IOException e) { throw new RuntimeExceptionWrapper(e); } formatMap.put(classIDObj,classFormat); } return classFormat; }

	 private ObjectStreamClass getClassFormat( byte[] classID, DatabaseEntry data) throws DatabaseException, ClassNotFoundException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getClassFormat__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private ClassInfo getClassInfo__wrappee__base( ObjectStreamClass classFormat) throws DatabaseException, ClassNotFoundException { String className=classFormat.getName(); ClassInfo classInfo=(ClassInfo)classMap.get(className); if (classInfo != null) { return classInfo; } else { char[] nameChars=className.toCharArray(); byte[] keyBytes=new byte[1 + UtfOps.getByteLength(nameChars)]; keyBytes[0]=REC_CLASS_INFO; UtfOps.charsToBytes(nameChars,0,keyBytes,1,nameChars.length); DatabaseEntry key=new DatabaseEntry(keyBytes); DatabaseEntry data=new DatabaseEntry(); OperationStatus status=db.get(null,key,data,LockMode.DEFAULT); if (status != OperationStatus.SUCCESS) { classInfo=putClassInfo(new ClassInfo(),className,key,classFormat); } else { classInfo=new ClassInfo(data); DatabaseEntry formatData=new DatabaseEntry(); ObjectStreamClass storedClassFormat=getClassFormat(classInfo.getClassID(),formatData); if (!areClassFormatsEqual(storedClassFormat,getBytes(formatData),classFormat)) { classInfo=putClassInfo(classInfo,className,key,classFormat); } classInfo.setClassFormat(classFormat); classMap.put(className,classInfo); } } return classInfo; }

	 private ClassInfo getClassInfo( ObjectStreamClass classFormat) throws DatabaseException, ClassNotFoundException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getClassInfo__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private ClassInfo putClassInfo__wrappee__base( ClassInfo classInfo, String className, DatabaseEntry classKey, ObjectStreamClass classFormat) throws DatabaseException, ClassNotFoundException { CursorConfig cursorConfig=null; if (cdbMode) { cursorConfig=new CursorConfig(); DbCompat.setWriteCursor(cursorConfig,true); } Cursor cursor=null; try { cursor=db.openCursor(null,cursorConfig); DatabaseEntry key=new DatabaseEntry(LAST_CLASS_ID_KEY); DatabaseEntry data=new DatabaseEntry(); OperationStatus status=cursor.getSearchKey(key,data,writeLockMode); if (status != OperationStatus.SUCCESS) { throw new IllegalStateException("Class ID not initialized"); } byte[] idBytes=getBytes(data); idBytes=incrementID(idBytes); data.setData(idBytes); cursor.put(key,data); byte[] keyBytes=new byte[1 + idBytes.length]; keyBytes[0]=REC_CLASS_FORMAT; System.arraycopy(idBytes,0,keyBytes,1,idBytes.length); key.setData(keyBytes); ByteArrayOutputStream baos=new ByteArrayOutputStream(); ObjectOutputStream oos; try { oos=new ObjectOutputStream(baos); oos.writeObject(classFormat); } catch ( IOException e) { throw new RuntimeExceptionWrapper(e); } data.setData(baos.toByteArray()); cursor.put(key,data); classInfo.setClassID(idBytes); classInfo.toDbt(data); cursor.put(classKey,data); classInfo.setClassFormat(classFormat); classMap.put(className,classInfo); formatMap.put(new BigInteger(idBytes),classFormat); return classInfo; } finally { if (cursor != null) { cursor.close(); } hook_commitTransaction(); } }

	 private ClassInfo putClassInfo( ClassInfo classInfo, String className, DatabaseEntry classKey, ObjectStreamClass classFormat) throws DatabaseException, ClassNotFoundException { t.in(Thread.currentThread().getStackTrace()[1].toString());	putClassInfo__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private void hook_commitTransaction__wrappee__base() throws DatabaseException { }

	 private void hook_commitTransaction() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook_commitTransaction__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private static byte\[\] incrementID__wrappee__base( byte[] key){ BigInteger id=new BigInteger(key); id=id.add(BigInteger.valueOf(1)); return id.toByteArray(); }

	 private static byte[] incrementID( byte[] key){ t.in(Thread.currentThread().getStackTrace()[1].toString());	incrementID__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private static boolean areClassFormatsEqual__wrappee__base( ObjectStreamClass format1, byte[] format1Bytes, ObjectStreamClass format2){ try { if (format1Bytes == null) { format1Bytes=getObjectBytes(format1); } byte[] format2Bytes=getObjectBytes(format2); return java.util.Arrays.equals(format2Bytes,format1Bytes); } catch ( IOException e) { return false; } }

	 private static boolean areClassFormatsEqual( ObjectStreamClass format1, byte[] format1Bytes, ObjectStreamClass format2){ t.in(Thread.currentThread().getStackTrace()[1].toString());	areClassFormatsEqual__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private static byte\[\] getBytes__wrappee__base( DatabaseEntry dbt){ byte[] b=dbt.getData(); if (b == null) { return null; } if (dbt.getOffset() == 0 && b.length == dbt.getSize()) { return b; } int len=dbt.getSize(); if (len == 0) { return ZERO_LENGTH_BYTE_ARRAY; } else { byte[] t=new byte[len]; System.arraycopy(b,dbt.getOffset(),t,0,t.length); return t; } }

	 private static byte[] getBytes( DatabaseEntry dbt){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBytes__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private static byte\[\] getObjectBytes__wrappee__base( Object o) throws IOException { ByteArrayOutputStream baos=new ByteArrayOutputStream(); ObjectOutputStream oos=new ObjectOutputStream(baos); oos.writeObject(o); return baos.toByteArray(); }

	 private static byte[] getObjectBytes( Object o) throws IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getObjectBytes__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
