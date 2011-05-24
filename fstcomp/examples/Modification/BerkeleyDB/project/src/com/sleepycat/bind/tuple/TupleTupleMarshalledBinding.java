package com.sleepycat.bind.tuple; 
import com.sleepycat.util.RuntimeExceptionWrapper; 
import de.ovgu.cide.jakutil.*; 
public  class  TupleTupleMarshalledBinding  extends TupleTupleBinding {
	 private Class cls;

	 public TupleTupleMarshalledBinding( Class cls){ this.cls=cls; if (!MarshalledTupleKeyEntity.class.isAssignableFrom(cls)) { throw new IllegalArgumentException(cls.toString() + " does not implement MarshalledTupleKeyEntity"); } if (!MarshalledTupleEntry.class.isAssignableFrom(cls)) { throw new IllegalArgumentException(cls.toString() + " does not implement MarshalledTupleEntry"); } }

	 public Object entryToObject__wrappee__base( TupleInput keyInput, TupleInput dataInput){ MarshalledTupleEntry obj; try { obj=(MarshalledTupleEntry)cls.newInstance(); } catch ( IllegalAccessException e) { throw new RuntimeExceptionWrapper(e); }
catch ( InstantiationException e) { throw new RuntimeExceptionWrapper(e); } if (dataInput != null) { obj.unmarshalEntry(dataInput); } MarshalledTupleKeyEntity entity=(MarshalledTupleKeyEntity)obj; if (keyInput != null) { entity.unmarshalPrimaryKey(keyInput); } return entity; }

	 public Object entryToObject( TupleInput keyInput, TupleInput dataInput){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToObject__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void objectToKey__wrappee__base( Object object, TupleOutput output){ MarshalledTupleKeyEntity entity=(MarshalledTupleKeyEntity)object; entity.marshalPrimaryKey(output); }

	 public void objectToKey( Object object, TupleOutput output){ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void objectToData__wrappee__base( Object object, TupleOutput output){ MarshalledTupleEntry entity=(MarshalledTupleEntry)object; entity.marshalEntry(output); }

	 public void objectToData( Object object, TupleOutput output){ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToData__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
