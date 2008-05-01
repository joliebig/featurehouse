package com.sleepycat.bind.tuple; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  class  ShortBinding  extends TupleBinding {
	 private static final int SHORT_SIZE=2;

	 public Object entryToObject( TupleInput input){ return new Short(input.readShort()); }

	 public void objectToEntry( Object object, TupleOutput output){ output.writeShort(((Number)object).shortValue()); }

	 protected TupleOutput getTupleOutput( Object object){ return sizedOutput(); }

	 public static short entryToShort( DatabaseEntry entry){ return entryToInput(entry).readShort(); }

	 public static void shortToEntry( short val, DatabaseEntry entry){ outputToEntry(sizedOutput().writeShort(val),entry); }

	 private static TupleOutput sizedOutput(){ return new TupleOutput(new byte[SHORT_SIZE]); }


}
