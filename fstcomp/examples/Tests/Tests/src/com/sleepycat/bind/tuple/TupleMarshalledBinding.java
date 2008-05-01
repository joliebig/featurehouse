package com.sleepycat.bind.tuple; 
import com.sleepycat.util.RuntimeExceptionWrapper; 
import de.ovgu.cide.jakutil.*; 
public  class  TupleMarshalledBinding  extends TupleBinding {
	 private Class cls;

	 public TupleMarshalledBinding( Class cls){ this.cls=cls; if (!MarshalledTupleEntry.class.isAssignableFrom(cls)) { throw new IllegalArgumentException(cls.toString() + " does not implement MarshalledTupleEntry"); } }

	 public Object entryToObject( TupleInput input){ try { MarshalledTupleEntry obj=(MarshalledTupleEntry)cls.newInstance(); obj.unmarshalEntry(input); return obj; } catch ( IllegalAccessException e) { throw new RuntimeExceptionWrapper(e); }
catch ( InstantiationException e) { throw new RuntimeExceptionWrapper(e); } }

	 public void objectToEntry( Object object, TupleOutput output){ MarshalledTupleEntry obj=(MarshalledTupleEntry)object; obj.marshalEntry(output); }


}
