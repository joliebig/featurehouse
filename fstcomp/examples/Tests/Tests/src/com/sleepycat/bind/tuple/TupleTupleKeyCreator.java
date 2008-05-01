package com.sleepycat.bind.tuple; 
import com.sleepycat.je.DatabaseEntry; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.ForeignKeyNullifier; 
import com.sleepycat.je.SecondaryDatabase; 
import com.sleepycat.je.SecondaryKeyCreator; 
import de.ovgu.cide.jakutil.*; 
public abstract  class  TupleTupleKeyCreator  extends TupleBase  implements SecondaryKeyCreator, ForeignKeyNullifier {
	 public TupleTupleKeyCreator(){ }

	 public boolean createSecondaryKey( SecondaryDatabase db, DatabaseEntry primaryKeyEntry, DatabaseEntry dataEntry, DatabaseEntry indexKeyEntry) throws DatabaseException { TupleOutput output=getTupleOutput(null); TupleInput primaryKeyInput=entryToInput(primaryKeyEntry); TupleInput dataInput=entryToInput(dataEntry); if (createSecondaryKey(primaryKeyInput,dataInput,output)) { outputToEntry(output,indexKeyEntry); return true; } else { return false; } }

	 public boolean nullifyForeignKey( SecondaryDatabase db, DatabaseEntry dataEntry) throws DatabaseException { TupleOutput output=getTupleOutput(null); if (nullifyForeignKey(entryToInput(dataEntry),output)) { outputToEntry(output,dataEntry); return true; } else { return false; } }

	 public abstract boolean createSecondaryKey( TupleInput primaryKeyInput, TupleInput dataInput, TupleOutput indexKeyOutput);

	 public boolean nullifyForeignKey( TupleInput dataInput, TupleOutput dataOutput){ return false; }


}
