package com.sleepycat.bind.tuple; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  class  CharacterBinding  extends TupleBinding {
	 private static final int CHAR_SIZE=2;

	 public Object entryToObject( TupleInput input){ return new Character(input.readChar()); }

	 public void objectToEntry( Object object, TupleOutput output){ output.writeChar(((Character)object).charValue()); }

	 protected TupleOutput getTupleOutput( Object object){ return sizedOutput(); }

	 public static char entryToChar( DatabaseEntry entry){ return entryToInput(entry).readChar(); }

	 public static void charToEntry( char val, DatabaseEntry entry){ outputToEntry(sizedOutput().writeChar(val),entry); }

	 private static TupleOutput sizedOutput(){ return new TupleOutput(new byte[CHAR_SIZE]); }


}
