package com.sleepycat.bind.serial; 
import com.sleepycat.bind.tuple.TupleBase; 
import com.sleepycat.bind.tuple.TupleInput; 
import com.sleepycat.bind.tuple.TupleOutput; 
import com.sleepycat.je.DatabaseEntry; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.ForeignKeyNullifier; 
import com.sleepycat.je.SecondaryDatabase; 
import com.sleepycat.je.SecondaryKeyCreator; 
import de.ovgu.cide.jakutil.*; 
public abstract  class  TupleSerialKeyCreator  extends TupleBase  implements SecondaryKeyCreator, ForeignKeyNullifier {
	 protected SerialBinding dataBinding;

	 public TupleSerialKeyCreator( ClassCatalog classCatalog, Class dataClass){ this(new SerialBinding(classCatalog,dataClass)); }

	 public TupleSerialKeyCreator( SerialBinding dataBinding){ this.dataBinding=dataBinding; }

	 public boolean createSecondaryKey( SecondaryDatabase db, DatabaseEntry primaryKeyEntry, DatabaseEntry dataEntry, DatabaseEntry indexKeyEntry) throws DatabaseException { TupleOutput output=getTupleOutput(null); TupleInput primaryKeyInput=entryToInput(primaryKeyEntry); Object dataInput=dataBinding.entryToObject(dataEntry); if (createSecondaryKey(primaryKeyInput,dataInput,output)) { outputToEntry(output,indexKeyEntry); return true; } else { return false; } }

	 public boolean nullifyForeignKey( SecondaryDatabase db, DatabaseEntry dataEntry) throws DatabaseException { Object data=dataBinding.entryToObject(dataEntry); data=nullifyForeignKey(data); if (data != null) { dataBinding.objectToEntry(data,dataEntry); return true; } else { return false; } }

	 public abstract boolean createSecondaryKey( TupleInput primaryKeyInput, Object dataInput, TupleOutput indexKeyOutput);

	 public Object nullifyForeignKey( Object data){ return null; }


}
