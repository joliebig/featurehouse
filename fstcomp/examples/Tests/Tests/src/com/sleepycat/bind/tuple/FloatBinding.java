package com.sleepycat.bind.tuple; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  class  FloatBinding  extends TupleBinding {
	 private static final int FLOAT_SIZE=4;

	 public Object entryToObject( TupleInput input){ return new Float(input.readFloat()); }

	 public void objectToEntry( Object object, TupleOutput output){ output.writeFloat(((Number)object).floatValue()); }

	 protected TupleOutput getTupleOutput( Object object){ return sizedOutput(); }

	 public static float entryToFloat( DatabaseEntry entry){ return entryToInput(entry).readFloat(); }

	 public static void floatToEntry( float val, DatabaseEntry entry){ outputToEntry(sizedOutput().writeFloat(val),entry); }

	 private static TupleOutput sizedOutput(){ return new TupleOutput(new byte[FLOAT_SIZE]); }


}
