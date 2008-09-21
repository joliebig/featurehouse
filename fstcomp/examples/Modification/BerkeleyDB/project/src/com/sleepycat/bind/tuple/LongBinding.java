package com.sleepycat.bind.tuple; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  class  LongBinding  extends TupleBinding {
	 private static final int LONG_SIZE=8;

	 public Object entryToObject__wrappee__base( TupleInput input){ return new Long(input.readLong()); }

	 public Object entryToObject( TupleInput input){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToObject__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void objectToEntry__wrappee__base( Object object, TupleOutput output){ output.writeLong(((Number)object).longValue()); }

	 public void objectToEntry( Object object, TupleOutput output){ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected TupleOutput getTupleOutput__wrappee__base( Object object){ return sizedOutput(); }

	 protected TupleOutput getTupleOutput( Object object){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTupleOutput__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static long entryToLong__wrappee__base( DatabaseEntry entry){ return entryToInput(entry).readLong(); }

	 public static long entryToLong( DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToLong__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void longToEntry__wrappee__base( long val, DatabaseEntry entry){ outputToEntry(sizedOutput().writeLong(val),entry); }

	 public static void longToEntry( long val, DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	longToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private static TupleOutput sizedOutput__wrappee__base(){ return new TupleOutput(new byte[LONG_SIZE]); }

	 private static TupleOutput sizedOutput(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	sizedOutput__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
