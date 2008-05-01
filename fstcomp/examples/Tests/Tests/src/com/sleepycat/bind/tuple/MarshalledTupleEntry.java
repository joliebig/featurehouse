package com.sleepycat.bind.tuple; 
import de.ovgu.cide.jakutil.*; 
public  interface  MarshalledTupleEntry {
	 void marshalEntry( TupleOutput dataOutput);

	 void unmarshalEntry( TupleInput dataInput);


}
