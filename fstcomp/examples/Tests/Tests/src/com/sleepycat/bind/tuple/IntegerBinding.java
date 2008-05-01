package com.sleepycat.bind.tuple; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  class  IntegerBinding  extends TupleBinding {
	 private static final int INT_SIZE=4;

	 public Object entryToObject( TupleInput input){ return new Integer(input.readInt()); }

	 public void objectToEntry( Object object, TupleOutput output){ output.writeInt(((Number)object).intValue()); }

	 protected TupleOutput getTupleOutput( Object object){ return sizedOutput(); }

	 public static int entryToInt( DatabaseEntry entry){ return entryToInput(entry).readInt(); }

	 public static void intToEntry( int val, DatabaseEntry entry){ outputToEntry(sizedOutput().writeInt(val),entry); }

	 private static TupleOutput sizedOutput(){ return new TupleOutput(new byte[INT_SIZE]); }


}
