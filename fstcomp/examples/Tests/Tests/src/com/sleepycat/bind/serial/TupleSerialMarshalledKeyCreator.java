package com.sleepycat.bind.serial; 
import com.sleepycat.bind.tuple.MarshalledTupleKeyEntity; 
import com.sleepycat.bind.tuple.TupleInput; 
import com.sleepycat.bind.tuple.TupleOutput; 
import de.ovgu.cide.jakutil.*; 
public  class  TupleSerialMarshalledKeyCreator  extends TupleSerialKeyCreator {
	 private TupleSerialMarshalledBinding binding;

	 private String keyName;

	 public TupleSerialMarshalledKeyCreator( TupleSerialMarshalledBinding binding, String keyName){ super(binding.dataBinding); this.binding=binding; this.keyName=keyName; if (dataBinding == null) { throw new NullPointerException("dataBinding may not be null"); } }

	 public boolean createSecondaryKey( TupleInput primaryKeyInput, Object dataInput, TupleOutput indexKeyOutput){ MarshalledTupleKeyEntity entity=(MarshalledTupleKeyEntity)binding.entryToObject(primaryKeyInput,dataInput); return entity.marshalSecondaryKey(keyName,indexKeyOutput); }

	 public Object nullifyForeignKey( Object dataInput){ MarshalledTupleKeyEntity entity=(MarshalledTupleKeyEntity)binding.entryToObject(null,dataInput); return entity.nullifyForeignKey(keyName) ? dataInput : null; }


}
