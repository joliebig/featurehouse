package com.sleepycat.bind.serial; 
import com.sleepycat.bind.EntityBinding; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public abstract  class  SerialSerialBinding  implements EntityBinding {
	 private SerialBinding keyBinding;

	 private SerialBinding dataBinding;

	 public SerialSerialBinding( ClassCatalog classCatalog, Class keyClass, Class dataClass){ this(new SerialBinding(classCatalog,keyClass),new SerialBinding(classCatalog,dataClass)); }

	 public SerialSerialBinding( SerialBinding keyBinding, SerialBinding dataBinding){ this.keyBinding=keyBinding; this.dataBinding=dataBinding; }

	 public Object entryToObject( DatabaseEntry key, DatabaseEntry data){ return entryToObject(keyBinding.entryToObject(key),dataBinding.entryToObject(data)); }

	 public void objectToKey( Object object, DatabaseEntry key){ object=objectToKey(object); keyBinding.objectToEntry(object,key); }

	 public void objectToData( Object object, DatabaseEntry data){ object=objectToData(object); dataBinding.objectToEntry(object,data); }

	 public abstract Object entryToObject( Object keyInput, Object dataInput);

	 public abstract Object objectToKey( Object object);

	 public abstract Object objectToData( Object object);


}
