package com.sleepycat.bind.serial; 
import com.sleepycat.je.DatabaseEntry; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.ForeignKeyNullifier; 
import com.sleepycat.je.SecondaryDatabase; 
import com.sleepycat.je.SecondaryKeyCreator; 
import de.ovgu.cide.jakutil.*; 
public abstract  class  SerialSerialKeyCreator  implements SecondaryKeyCreator, ForeignKeyNullifier {
	 protected SerialBinding primaryKeyBinding;

	 protected SerialBinding dataBinding;

	 protected SerialBinding indexKeyBinding;

	 public SerialSerialKeyCreator( ClassCatalog classCatalog, Class primaryKeyClass, Class dataClass, Class indexKeyClass){ this(new SerialBinding(classCatalog,primaryKeyClass),new SerialBinding(classCatalog,dataClass),new SerialBinding(classCatalog,indexKeyClass)); }

	 public SerialSerialKeyCreator( SerialBinding primaryKeyBinding, SerialBinding dataBinding, SerialBinding indexKeyBinding){ this.primaryKeyBinding=primaryKeyBinding; this.dataBinding=dataBinding; this.indexKeyBinding=indexKeyBinding; }

	 public boolean createSecondaryKey__wrappee__base( SecondaryDatabase db, DatabaseEntry primaryKeyEntry, DatabaseEntry dataEntry, DatabaseEntry indexKeyEntry) throws DatabaseException { Object primaryKeyInput=primaryKeyBinding.entryToObject(primaryKeyEntry); Object dataInput=dataBinding.entryToObject(dataEntry); Object indexKey=createSecondaryKey(primaryKeyInput,dataInput); if (indexKey != null) { indexKeyBinding.objectToEntry(indexKey,indexKeyEntry); return true; } else { return false; } }

	 public boolean createSecondaryKey( SecondaryDatabase db, DatabaseEntry primaryKeyEntry, DatabaseEntry dataEntry, DatabaseEntry indexKeyEntry) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	createSecondaryKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean nullifyForeignKey__wrappee__base( SecondaryDatabase db, DatabaseEntry dataEntry) throws DatabaseException { Object data=dataBinding.entryToObject(dataEntry); data=nullifyForeignKey(data); if (data != null) { dataBinding.objectToEntry(data,dataEntry); return true; } else { return false; } }

	 public boolean nullifyForeignKey( SecondaryDatabase db, DatabaseEntry dataEntry) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	nullifyForeignKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract Object createSecondaryKey__wrappee__base( Object primaryKey, Object data);

	 public abstract Object createSecondaryKey( Object primaryKey, Object data);{ t.in(Thread.currentThread().getStackTrace()[1].toString());	createSecondaryKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Object nullifyForeignKey__wrappee__base( Object data){ return null; }

	 public Object nullifyForeignKey( Object data){ t.in(Thread.currentThread().getStackTrace()[1].toString());	nullifyForeignKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
