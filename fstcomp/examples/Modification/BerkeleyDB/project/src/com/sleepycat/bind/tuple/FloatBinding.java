package com.sleepycat.bind.tuple; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  class  FloatBinding  extends TupleBinding {
	 private static final int FLOAT_SIZE=4;

	 public Object entryToObject__wrappee__base( TupleInput input){ return new Float(input.readFloat()); }

	 public Object entryToObject( TupleInput input){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToObject__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void objectToEntry__wrappee__base( Object object, TupleOutput output){ output.writeFloat(((Number)object).floatValue()); }

	 public void objectToEntry( Object object, TupleOutput output){ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected TupleOutput getTupleOutput__wrappee__base( Object object){ return sizedOutput(); }

	 protected TupleOutput getTupleOutput( Object object){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTupleOutput__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static float entryToFloat__wrappee__base( DatabaseEntry entry){ return entryToInput(entry).readFloat(); }

	 public static float entryToFloat( DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToFloat__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void floatToEntry__wrappee__base( float val, DatabaseEntry entry){ outputToEntry(sizedOutput().writeFloat(val),entry); }

	 public static void floatToEntry( float val, DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	floatToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private static TupleOutput sizedOutput__wrappee__base(){ return new TupleOutput(new byte[FLOAT_SIZE]); }

	 private static TupleOutput sizedOutput(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	sizedOutput__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
