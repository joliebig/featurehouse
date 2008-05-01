package com.sleepycat.bind.tuple; 
import com.sleepycat.bind.EntityBinding; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public abstract  class  TupleTupleBinding  extends TupleBase  implements EntityBinding {
	 public TupleTupleBinding(){ }

	 public Object entryToObject( DatabaseEntry key, DatabaseEntry data){ return entryToObject(TupleBinding.entryToInput(key),TupleBinding.entryToInput(data)); }

	 public void objectToKey( Object object, DatabaseEntry key){ TupleOutput output=getTupleOutput(object); objectToKey(object,output); outputToEntry(output,key); }

	 public void objectToData( Object object, DatabaseEntry data){ TupleOutput output=getTupleOutput(object); objectToData(object,output); outputToEntry(output,data); }

	 public abstract Object entryToObject( TupleInput keyInput, TupleInput dataInput);

	 public abstract void objectToKey( Object object, TupleOutput output);

	 public abstract void objectToData( Object object, TupleOutput output);


}
