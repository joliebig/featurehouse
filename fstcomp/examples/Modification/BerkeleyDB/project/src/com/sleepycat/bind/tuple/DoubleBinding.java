package com.sleepycat.bind.tuple; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  class  DoubleBinding  extends TupleBinding {
	 private static final int DOUBLE_SIZE=8;

	 public Object entryToObject__wrappee__base( TupleInput input){ return new Double(input.readDouble()); }

	 public Object entryToObject( TupleInput input){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToObject__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void objectToEntry__wrappee__base( Object object, TupleOutput output){ output.writeDouble(((Number)object).doubleValue()); }

	 public void objectToEntry( Object object, TupleOutput output){ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected TupleOutput getTupleOutput__wrappee__base( Object object){ return sizedOutput(); }

	 protected TupleOutput getTupleOutput( Object object){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTupleOutput__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static double entryToDouble__wrappee__base( DatabaseEntry entry){ return entryToInput(entry).readDouble(); }

	 public static double entryToDouble( DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToDouble__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void doubleToEntry__wrappee__base( double val, DatabaseEntry entry){ outputToEntry(sizedOutput().writeDouble(val),entry); }

	 public static void doubleToEntry( double val, DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	doubleToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private static TupleOutput sizedOutput__wrappee__base(){ return new TupleOutput(new byte[DOUBLE_SIZE]); }

	 private static TupleOutput sizedOutput(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	sizedOutput__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
