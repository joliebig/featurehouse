package com.sleepycat.bind.tuple; 
import com.sleepycat.je.DatabaseEntry; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.ForeignKeyNullifier; 
import com.sleepycat.je.SecondaryDatabase; 
import com.sleepycat.je.SecondaryKeyCreator; 
import de.ovgu.cide.jakutil.*; 
public abstract  class  TupleTupleKeyCreator  extends TupleBase  implements SecondaryKeyCreator, ForeignKeyNullifier {
	 public TupleTupleKeyCreator(){ }

	 public boolean createSecondaryKey__wrappee__base( SecondaryDatabase db, DatabaseEntry primaryKeyEntry, DatabaseEntry dataEntry, DatabaseEntry indexKeyEntry) throws DatabaseException { TupleOutput output=getTupleOutput(null); TupleInput primaryKeyInput=entryToInput(primaryKeyEntry); TupleInput dataInput=entryToInput(dataEntry); if (createSecondaryKey(primaryKeyInput,dataInput,output)) { outputToEntry(output,indexKeyEntry); return true; } else { return false; } }

	 public boolean createSecondaryKey( SecondaryDatabase db, DatabaseEntry primaryKeyEntry, DatabaseEntry dataEntry, DatabaseEntry indexKeyEntry) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	createSecondaryKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean nullifyForeignKey__wrappee__base( SecondaryDatabase db, DatabaseEntry dataEntry) throws DatabaseException { TupleOutput output=getTupleOutput(null); if (nullifyForeignKey(entryToInput(dataEntry),output)) { outputToEntry(output,dataEntry); return true; } else { return false; } }

	 public boolean nullifyForeignKey( SecondaryDatabase db, DatabaseEntry dataEntry) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	nullifyForeignKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract boolean createSecondaryKey__wrappee__base( TupleInput primaryKeyInput, TupleInput dataInput, TupleOutput indexKeyOutput);

	 public abstract boolean createSecondaryKey( TupleInput primaryKeyInput, TupleInput dataInput, TupleOutput indexKeyOutput);{ t.in(Thread.currentThread().getStackTrace()[1].toString());	createSecondaryKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean nullifyForeignKey__wrappee__base( TupleInput dataInput, TupleOutput dataOutput){ return false; }

	 public boolean nullifyForeignKey( TupleInput dataInput, TupleOutput dataOutput){ t.in(Thread.currentThread().getStackTrace()[1].toString());	nullifyForeignKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
