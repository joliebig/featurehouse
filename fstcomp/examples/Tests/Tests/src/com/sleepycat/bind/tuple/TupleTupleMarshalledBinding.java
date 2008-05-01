package com.sleepycat.bind.tuple; 
import com.sleepycat.util.RuntimeExceptionWrapper; 
import de.ovgu.cide.jakutil.*; 
public  class  TupleTupleMarshalledBinding  extends TupleTupleBinding {
	 private Class cls;

	 public TupleTupleMarshalledBinding( Class cls){ this.cls=cls; if (!MarshalledTupleKeyEntity.class.isAssignableFrom(cls)) { throw new IllegalArgumentException(cls.toString() + " does not implement MarshalledTupleKeyEntity"); } if (!MarshalledTupleEntry.class.isAssignableFrom(cls)) { throw new IllegalArgumentException(cls.toString() + " does not implement MarshalledTupleEntry"); } }

	 public Object entryToObject( TupleInput keyInput, TupleInput dataInput){ MarshalledTupleEntry obj; try { obj=(MarshalledTupleEntry)cls.newInstance(); } catch ( IllegalAccessException e) { throw new RuntimeExceptionWrapper(e); }
catch ( InstantiationException e) { throw new RuntimeExceptionWrapper(e); } if (dataInput != null) { obj.unmarshalEntry(dataInput); } MarshalledTupleKeyEntity entity=(MarshalledTupleKeyEntity)obj; if (keyInput != null) { entity.unmarshalPrimaryKey(keyInput); } return entity; }

	 public void objectToKey( Object object, TupleOutput output){ MarshalledTupleKeyEntity entity=(MarshalledTupleKeyEntity)object; entity.marshalPrimaryKey(output); }

	 public void objectToData( Object object, TupleOutput output){ MarshalledTupleEntry entity=(MarshalledTupleEntry)object; entity.marshalEntry(output); }


}
