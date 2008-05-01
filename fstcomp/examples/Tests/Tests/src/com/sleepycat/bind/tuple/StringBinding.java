package com.sleepycat.bind.tuple; 
import com.sleepycat.je.DatabaseEntry; 
import com.sleepycat.util.UtfOps; 
import de.ovgu.cide.jakutil.*; 
public  class  StringBinding  extends TupleBinding {
	 public Object entryToObject( TupleInput input){ return input.readString(); }

	 public void objectToEntry( Object object, TupleOutput output){ output.writeString((String)object); }

	 protected TupleOutput getTupleOutput( Object object){ return sizedOutput((String)object); }

	 public static String entryToString( DatabaseEntry entry){ return entryToInput(entry).readString(); }

	 public static void stringToEntry( String val, DatabaseEntry entry){ outputToEntry(sizedOutput(val).writeString(val),entry); }

	 private static TupleOutput sizedOutput( String val){ int stringLength=(val == null) ? 1 : UtfOps.getByteLength(val.toCharArray()); stringLength++; return new TupleOutput(new byte[stringLength]); }


}
