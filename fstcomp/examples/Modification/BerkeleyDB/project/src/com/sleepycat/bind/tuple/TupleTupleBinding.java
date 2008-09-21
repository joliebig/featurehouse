package com.sleepycat.bind.tuple; 
import com.sleepycat.bind.EntityBinding; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public abstract  class  TupleTupleBinding  extends TupleBase  implements EntityBinding {
	 public TupleTupleBinding(){ }

	 public Object entryToObject__wrappee__base( DatabaseEntry key, DatabaseEntry data){ return entryToObject(TupleBinding.entryToInput(key),TupleBinding.entryToInput(data)); }

	 public Object entryToObject( DatabaseEntry key, DatabaseEntry data){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToObject__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void objectToKey__wrappee__base( Object object, DatabaseEntry key){ TupleOutput output=getTupleOutput(object); objectToKey(object,output); outputToEntry(output,key); }

	 public void objectToKey( Object object, DatabaseEntry key){ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void objectToData__wrappee__base( Object object, DatabaseEntry data){ TupleOutput output=getTupleOutput(object); objectToData(object,output); outputToEntry(output,data); }

	 public void objectToData( Object object, DatabaseEntry data){ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToData__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract Object entryToObject__wrappee__base( TupleInput keyInput, TupleInput dataInput);

	 public abstract Object entryToObject( TupleInput keyInput, TupleInput dataInput);{ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToObject__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract void objectToKey__wrappee__base( Object object, TupleOutput output);

	 public abstract void objectToKey( Object object, TupleOutput output);{ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract void objectToData__wrappee__base( Object object, TupleOutput output);

	 public abstract void objectToData( Object object, TupleOutput output);{ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToData__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
