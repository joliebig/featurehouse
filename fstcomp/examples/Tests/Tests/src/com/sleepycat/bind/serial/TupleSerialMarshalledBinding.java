package com.sleepycat.bind.serial; 
import com.sleepycat.bind.tuple.MarshalledTupleKeyEntity; 
import com.sleepycat.bind.tuple.TupleInput; 
import com.sleepycat.bind.tuple.TupleOutput; 
import de.ovgu.cide.jakutil.*; 
public  class  TupleSerialMarshalledBinding  extends TupleSerialBinding {
	 public TupleSerialMarshalledBinding( ClassCatalog classCatalog, Class baseClass){ this(new SerialBinding(classCatalog,baseClass)); }

	 public TupleSerialMarshalledBinding( SerialBinding dataBinding){ super(dataBinding); }

	 public Object entryToObject( TupleInput tupleInput, Object javaInput){ MarshalledTupleKeyEntity entity=(MarshalledTupleKeyEntity)javaInput; if (tupleInput != null) { entity.unmarshalPrimaryKey(tupleInput); } return entity; }

	 public void objectToKey( Object object, TupleOutput output){ MarshalledTupleKeyEntity entity=(MarshalledTupleKeyEntity)object; entity.marshalPrimaryKey(output); }

	 public Object objectToData( Object object){ return object; }


}
