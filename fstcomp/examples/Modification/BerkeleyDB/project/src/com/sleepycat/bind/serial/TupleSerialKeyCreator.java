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

	 public boolean createSecondaryKey__wrappee__base( SecondaryDatabase db, DatabaseEntry primaryKeyEntry, DatabaseEntry dataEntry, DatabaseEntry indexKeyEntry) throws DatabaseException { TupleOutput output=getTupleOutput(null); TupleInput primaryKeyInput=entryToInput(primaryKeyEntry); Object dataInput=dataBinding.entryToObject(dataEntry); if (createSecondaryKey(primaryKeyInput,dataInput,output)) { outputToEntry(output,indexKeyEntry); return true; } else { return false; } }

	 public boolean createSecondaryKey( SecondaryDatabase db, DatabaseEntry primaryKeyEntry, DatabaseEntry dataEntry, DatabaseEntry indexKeyEntry) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	createSecondaryKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean nullifyForeignKey__wrappee__base( SecondaryDatabase db, DatabaseEntry dataEntry) throws DatabaseException { Object data=dataBinding.entryToObject(dataEntry); data=nullifyForeignKey(data); if (data != null) { dataBinding.objectToEntry(data,dataEntry); return true; } else { return false; } }

	 public boolean nullifyForeignKey( SecondaryDatabase db, DatabaseEntry dataEntry) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	nullifyForeignKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract boolean createSecondaryKey__wrappee__base( TupleInput primaryKeyInput, Object dataInput, TupleOutput indexKeyOutput);

	 public abstract boolean createSecondaryKey( TupleInput primaryKeyInput, Object dataInput, TupleOutput indexKeyOutput);{ t.in(Thread.currentThread().getStackTrace()[1].toString());	createSecondaryKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Object nullifyForeignKey__wrappee__base( Object data){ return null; }

	 public Object nullifyForeignKey( Object data){ t.in(Thread.currentThread().getStackTrace()[1].toString());	nullifyForeignKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
