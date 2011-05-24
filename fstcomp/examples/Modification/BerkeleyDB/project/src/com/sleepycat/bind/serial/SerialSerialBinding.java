package com.sleepycat.bind.serial; 
import com.sleepycat.bind.EntityBinding; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public abstract  class  SerialSerialBinding  implements EntityBinding {
	 private SerialBinding keyBinding;

	 private SerialBinding dataBinding;

	 public SerialSerialBinding( ClassCatalog classCatalog, Class keyClass, Class dataClass){ this(new SerialBinding(classCatalog,keyClass),new SerialBinding(classCatalog,dataClass)); }

	 public SerialSerialBinding( SerialBinding keyBinding, SerialBinding dataBinding){ this.keyBinding=keyBinding; this.dataBinding=dataBinding; }

	 public Object entryToObject__wrappee__base( DatabaseEntry key, DatabaseEntry data){ return entryToObject(keyBinding.entryToObject(key),dataBinding.entryToObject(data)); }

	 public Object entryToObject( DatabaseEntry key, DatabaseEntry data){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToObject__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void objectToKey__wrappee__base( Object object, DatabaseEntry key){ object=objectToKey(object); keyBinding.objectToEntry(object,key); }

	 public void objectToKey( Object object, DatabaseEntry key){ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void objectToData__wrappee__base( Object object, DatabaseEntry data){ object=objectToData(object); dataBinding.objectToEntry(object,data); }

	 public void objectToData( Object object, DatabaseEntry data){ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToData__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract Object entryToObject__wrappee__base( Object keyInput, Object dataInput);

	 public abstract Object entryToObject( Object keyInput, Object dataInput);{ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToObject__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract Object objectToKey__wrappee__base( Object object);

	 public abstract Object objectToKey( Object object);{ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract Object objectToData__wrappee__base( Object object);

	 public abstract Object objectToData( Object object);{ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToData__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
