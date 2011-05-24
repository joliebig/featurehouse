package com.sleepycat.bind.tuple; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  class  CharacterBinding  extends TupleBinding {
	 private static final int CHAR_SIZE=2;

	 public Object entryToObject__wrappee__base( TupleInput input){ return new Character(input.readChar()); }

	 public Object entryToObject( TupleInput input){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToObject__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void objectToEntry__wrappee__base( Object object, TupleOutput output){ output.writeChar(((Character)object).charValue()); }

	 public void objectToEntry( Object object, TupleOutput output){ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected TupleOutput getTupleOutput__wrappee__base( Object object){ return sizedOutput(); }

	 protected TupleOutput getTupleOutput( Object object){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTupleOutput__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static char entryToChar__wrappee__base( DatabaseEntry entry){ return entryToInput(entry).readChar(); }

	 public static char entryToChar( DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToChar__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void charToEntry__wrappee__base( char val, DatabaseEntry entry){ outputToEntry(sizedOutput().writeChar(val),entry); }

	 public static void charToEntry( char val, DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	charToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private static TupleOutput sizedOutput__wrappee__base(){ return new TupleOutput(new byte[CHAR_SIZE]); }

	 private static TupleOutput sizedOutput(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	sizedOutput__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
