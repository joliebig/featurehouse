package com.sleepycat.bind.tuple; 
import de.ovgu.cide.jakutil.*; 
public  class  TupleTupleMarshalledKeyCreator  extends TupleTupleKeyCreator {
	 private String keyName;

	 private TupleTupleMarshalledBinding binding;

	 public TupleTupleMarshalledKeyCreator( TupleTupleMarshalledBinding binding, String keyName){ this.binding=binding; this.keyName=keyName; }

	 public boolean createSecondaryKey( TupleInput primaryKeyInput, TupleInput dataInput, TupleOutput indexKeyOutput){ MarshalledTupleKeyEntity entity=(MarshalledTupleKeyEntity)binding.entryToObject(primaryKeyInput,dataInput); return entity.marshalSecondaryKey(keyName,indexKeyOutput); }

	 public boolean nullifyForeignKey( TupleInput dataInput, TupleOutput dataOutput){ MarshalledTupleKeyEntity entity=(MarshalledTupleKeyEntity)binding.entryToObject(null,dataInput); if (entity.nullifyForeignKey(keyName)) { binding.objectToData(entity,dataOutput); return true; } else { return false; } }


}
