package com.sleepycat.bind.tuple; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  class  IntegerBinding  extends TupleBinding {
	 private static final int INT_SIZE=4;

	 public Object entryToObject__wrappee__base( TupleInput input){ return new Integer(input.readInt()); }

	 public Object entryToObject( TupleInput input){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToObject__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void objectToEntry__wrappee__base( Object object, TupleOutput output){ output.writeInt(((Number)object).intValue()); }

	 public void objectToEntry( Object object, TupleOutput output){ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected TupleOutput getTupleOutput__wrappee__base( Object object){ return sizedOutput(); }

	 protected TupleOutput getTupleOutput( Object object){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTupleOutput__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static int entryToInt__wrappee__base( DatabaseEntry entry){ return entryToInput(entry).readInt(); }

	 public static int entryToInt( DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToInt__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void intToEntry__wrappee__base( int val, DatabaseEntry entry){ outputToEntry(sizedOutput().writeInt(val),entry); }

	 public static void intToEntry( int val, DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	intToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private static TupleOutput sizedOutput__wrappee__base(){ return new TupleOutput(new byte[INT_SIZE]); }

	 private static TupleOutput sizedOutput(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	sizedOutput__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
