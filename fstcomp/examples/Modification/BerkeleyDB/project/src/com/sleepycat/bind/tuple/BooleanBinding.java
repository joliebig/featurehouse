package com.sleepycat.bind.tuple; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  class  BooleanBinding  extends TupleBinding {
	 private static final int BOOLEAN_SIZE=1;

	 public Object entryToObject__wrappee__base( TupleInput input){ return input.readBoolean() ? Boolean.TRUE : Boolean.FALSE; }

	 public Object entryToObject( TupleInput input){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToObject__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void objectToEntry__wrappee__base( Object object, TupleOutput output){ output.writeBoolean(((Boolean)object).booleanValue()); }

	 public void objectToEntry( Object object, TupleOutput output){ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected TupleOutput getTupleOutput__wrappee__base( Object object){ return sizedOutput(); }

	 protected TupleOutput getTupleOutput( Object object){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTupleOutput__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static boolean entryToBoolean__wrappee__base( DatabaseEntry entry){ return entryToInput(entry).readBoolean(); }

	 public static boolean entryToBoolean( DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToBoolean__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void booleanToEntry__wrappee__base( boolean val, DatabaseEntry entry){ outputToEntry(sizedOutput().writeBoolean(val),entry); }

	 public static void booleanToEntry( boolean val, DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	booleanToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private static TupleOutput sizedOutput__wrappee__base(){ return new TupleOutput(new byte[BOOLEAN_SIZE]); }

	 private static TupleOutput sizedOutput(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	sizedOutput__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
