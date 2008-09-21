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

	 public Object entryToObject__wrappee__base( DatabaseEntry key, DatabaseEntry data){ return entryToObject(entryToInput(key),dataBinding.entryToObject(data)); }

	 public Object entryToObject( DatabaseEntry key, DatabaseEntry data){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToObject__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void objectToKey__wrappee__base( Object object, DatabaseEntry key){ TupleOutput output=getTupleOutput(object); objectToKey(object,output); outputToEntry(output,key); }

	 public void objectToKey( Object object, DatabaseEntry key){ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void objectToData__wrappee__base( Object object, DatabaseEntry data){ object=objectToData(object); dataBinding.objectToEntry(object,data); }

	 public void objectToData( Object object, DatabaseEntry data){ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToData__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract Object entryToObject__wrappee__base( TupleInput keyInput, Object dataInput);

	 public abstract Object entryToObject( TupleInput keyInput, Object dataInput);{ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToObject__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract void objectToKey__wrappee__base( Object object, TupleOutput keyOutput);

	 public abstract void objectToKey( Object object, TupleOutput keyOutput);{ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract Object objectToData__wrappee__base( Object object);

	 public abstract Object objectToData( Object object);{ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToData__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
