package com.sleepycat.bind.tuple; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  class  DoubleBinding  extends TupleBinding {
	 private static final int DOUBLE_SIZE=8;

	 public Object entryToObject( TupleInput input){ return new Double(input.readDouble()); }

	 public void objectToEntry( Object object, TupleOutput output){ output.writeDouble(((Number)object).doubleValue()); }

	 protected TupleOutput getTupleOutput( Object object){ return sizedOutput(); }

	 public static double entryToDouble( DatabaseEntry entry){ return entryToInput(entry).readDouble(); }

	 public static void doubleToEntry( double val, DatabaseEntry entry){ outputToEntry(sizedOutput().writeDouble(val),entry); }

	 private static TupleOutput sizedOutput(){ return new TupleOutput(new byte[DOUBLE_SIZE]); }


}
