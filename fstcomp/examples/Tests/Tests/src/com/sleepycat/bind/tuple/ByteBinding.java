package com.sleepycat.bind.tuple; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  class  ByteBinding  extends TupleBinding {
	 private static final int BYTE_SIZE=1;

	 public Object entryToObject( TupleInput input){ return new Byte(input.readByte()); }

	 public void objectToEntry( Object object, TupleOutput output){ output.writeByte(((Number)object).byteValue()); }

	 protected TupleOutput getTupleOutput( Object object){ return sizedOutput(); }

	 public static byte entryToByte( DatabaseEntry entry){ return entryToInput(entry).readByte(); }

	 public static void byteToEntry( byte val, DatabaseEntry entry){ outputToEntry(sizedOutput().writeByte(val),entry); }

	 private static TupleOutput sizedOutput(){ return new TupleOutput(new byte[BYTE_SIZE]); }


}
