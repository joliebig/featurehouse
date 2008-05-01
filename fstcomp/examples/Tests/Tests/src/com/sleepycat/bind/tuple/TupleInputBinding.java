package com.sleepycat.bind.tuple; 
import com.sleepycat.bind.EntryBinding; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  class  TupleInputBinding  implements EntryBinding {
	 public TupleInputBinding(){ }

	 public Object entryToObject( DatabaseEntry entry){ return TupleBinding.entryToInput(entry); }

	 public void objectToEntry( Object object, DatabaseEntry entry){ TupleBinding.inputToEntry((TupleInput)object,entry); }


}
