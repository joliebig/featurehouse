package com.sleepycat.bind.tuple; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  class  BooleanBinding  extends TupleBinding {
	 private static final int BOOLEAN_SIZE=1;

	 public Object entryToObject( TupleInput input){ return input.readBoolean() ? Boolean.TRUE : Boolean.FALSE; }

	 public void objectToEntry( Object object, TupleOutput output){ output.writeBoolean(((Boolean)object).booleanValue()); }

	 protected TupleOutput getTupleOutput( Object object){ return sizedOutput(); }

	 public static boolean entryToBoolean( DatabaseEntry entry){ return entryToInput(entry).readBoolean(); }

	 public static void booleanToEntry( boolean val, DatabaseEntry entry){ outputToEntry(sizedOutput().writeBoolean(val),entry); }

	 private static TupleOutput sizedOutput(){ return new TupleOutput(new byte[BOOLEAN_SIZE]); }


}
