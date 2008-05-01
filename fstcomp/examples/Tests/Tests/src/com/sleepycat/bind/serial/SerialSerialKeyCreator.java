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

	 public boolean createSecondaryKey( SecondaryDatabase db, DatabaseEntry primaryKeyEntry, DatabaseEntry dataEntry, DatabaseEntry indexKeyEntry) throws DatabaseException { Object primaryKeyInput=primaryKeyBinding.entryToObject(primaryKeyEntry); Object dataInput=dataBinding.entryToObject(dataEntry); Object indexKey=createSecondaryKey(primaryKeyInput,dataInput); if (indexKey != null) { indexKeyBinding.objectToEntry(indexKey,indexKeyEntry); return true; } else { return false; } }

	 public boolean nullifyForeignKey( SecondaryDatabase db, DatabaseEntry dataEntry) throws DatabaseException { Object data=dataBinding.entryToObject(dataEntry); data=nullifyForeignKey(data); if (data != null) { dataBinding.objectToEntry(data,dataEntry); return true; } else { return false; } }

	 public abstract Object createSecondaryKey( Object primaryKey, Object data);

	 public Object nullifyForeignKey( Object data){ return null; }


}
