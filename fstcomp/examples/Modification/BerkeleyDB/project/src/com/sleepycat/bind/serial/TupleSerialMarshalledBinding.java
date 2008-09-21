package com.sleepycat.bind.serial; 
import com.sleepycat.bind.tuple.MarshalledTupleKeyEntity; 
import com.sleepycat.bind.tuple.TupleInput; 
import com.sleepycat.bind.tuple.TupleOutput; 
import de.ovgu.cide.jakutil.*; 
public  class  TupleSerialMarshalledBinding  extends TupleSerialBinding {
	 public TupleSerialMarshalledBinding( ClassCatalog classCatalog, Class baseClass){ this(new SerialBinding(classCatalog,baseClass)); }

	 public TupleSerialMarshalledBinding( SerialBinding dataBinding){ super(dataBinding); }

	 public Object entryToObject__wrappee__base( TupleInput tupleInput, Object javaInput){ MarshalledTupleKeyEntity entity=(MarshalledTupleKeyEntity)javaInput; if (tupleInput != null) { entity.unmarshalPrimaryKey(tupleInput); } return entity; }

	 public Object entryToObject( TupleInput tupleInput, Object javaInput){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToObject__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void objectToKey__wrappee__base( Object object, TupleOutput output){ MarshalledTupleKeyEntity entity=(MarshalledTupleKeyEntity)object; entity.marshalPrimaryKey(output); }

	 public void objectToKey( Object object, TupleOutput output){ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Object objectToData__wrappee__base( Object object){ return object; }

	 public Object objectToData( Object object){ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToData__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
