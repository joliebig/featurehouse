package com.sleepycat.bind.tuple; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  class  LongBinding  extends TupleBinding {
	 private static final int LONG_SIZE=8;

	 public Object entryToObject( TupleInput input){ return new Long(input.readLong()); }

	 public void objectToEntry( Object object, TupleOutput output){ output.writeLong(((Number)object).longValue()); }

	 protected TupleOutput getTupleOutput( Object object){ return sizedOutput(); }

	 public static long entryToLong( DatabaseEntry entry){ return entryToInput(entry).readLong(); }

	 public static void longToEntry( long val, DatabaseEntry entry){ outputToEntry(sizedOutput().writeLong(val),entry); }

	 private static TupleOutput sizedOutput(){ return new TupleOutput(new byte[LONG_SIZE]); }


}
