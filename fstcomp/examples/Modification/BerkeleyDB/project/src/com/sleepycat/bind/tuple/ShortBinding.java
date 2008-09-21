package com.sleepycat.bind.tuple; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  class  ShortBinding  extends TupleBinding {
	 private static final int SHORT_SIZE=2;

	 public Object entryToObject__wrappee__base( TupleInput input){ return new Short(input.readShort()); }

	 public Object entryToObject( TupleInput input){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToObject__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void objectToEntry__wrappee__base( Object object, TupleOutput output){ output.writeShort(((Number)object).shortValue()); }

	 public void objectToEntry( Object object, TupleOutput output){ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected TupleOutput getTupleOutput__wrappee__base( Object object){ return sizedOutput(); }

	 protected TupleOutput getTupleOutput( Object object){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTupleOutput__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static short entryToShort__wrappee__base( DatabaseEntry entry){ return entryToInput(entry).readShort(); }

	 public static short entryToShort( DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToShort__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void shortToEntry__wrappee__base( short val, DatabaseEntry entry){ outputToEntry(sizedOutput().writeShort(val),entry); }

	 public static void shortToEntry( short val, DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	shortToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private static TupleOutput sizedOutput__wrappee__base(){ return new TupleOutput(new byte[SHORT_SIZE]); }

	 private static TupleOutput sizedOutput(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	sizedOutput__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
