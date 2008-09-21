package com.sleepycat.bind.tuple; 
import de.ovgu.cide.jakutil.*; 
public  interface  MarshalledTupleKeyEntity {
	 void marshalPrimaryKey( TupleOutput keyOutput);

	 void unmarshalPrimaryKey( TupleInput keyInput);

	 boolean marshalSecondaryKey( String keyName, TupleOutput keyOutput);

	 boolean nullifyForeignKey( String keyName);


}
