package com.sleepycat.bind.serial; 
import com.sleepycat.bind.EntityBinding; 
import com.sleepycat.bind.tuple.TupleBase; 
import com.sleepycat.bind.tuple.TupleInput; 
import com.sleepycat.bind.tuple.TupleOutput; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public abstract  class  TupleSerialBinding  extends TupleBase  implements EntityBinding {
	 protected SerialBinding dataBinding;

	 public TupleSerialBinding( ClassCatalog classCatalog, Class baseClass){ this(new SerialBinding(classCatalog,baseClass)); }

	 public TupleSerialBinding( SerialBinding dataBinding){ this.dataBinding=dataBinding; }

	 public Object entryToObject( DatabaseEntry key, DatabaseEntry data){ return entryToObject(entryToInput(key),dataBinding.entryToObject(data)); }

	 public void objectToKey( Object object, DatabaseEntry key){ TupleOutput output=getTupleOutput(object); objectToKey(object,output); outputToEntry(output,key); }

	 public void objectToData( Object object, DatabaseEntry data){ object=objectToData(object); dataBinding.objectToEntry(object,data); }

	 public abstract Object entryToObject( TupleInput keyInput, Object dataInput);

	 public abstract void objectToKey( Object object, TupleOutput keyOutput);

	 public abstract Object objectToData( Object object);


}
