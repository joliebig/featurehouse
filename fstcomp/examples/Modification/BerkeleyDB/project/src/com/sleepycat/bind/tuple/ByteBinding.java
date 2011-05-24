package com.sleepycat.bind.tuple; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  class  ByteBinding  extends TupleBinding {
	 private static final int BYTE_SIZE=1;

	 public Object entryToObject__wrappee__base( TupleInput input){ return new Byte(input.readByte()); }

	 public Object entryToObject( TupleInput input){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToObject__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void objectToEntry__wrappee__base( Object object, TupleOutput output){ output.writeByte(((Number)object).byteValue()); }

	 public void objectToEntry( Object object, TupleOutput output){ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected TupleOutput getTupleOutput__wrappee__base( Object object){ return sizedOutput(); }

	 protected TupleOutput getTupleOutput( Object object){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTupleOutput__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static byte entryToByte__wrappee__base( DatabaseEntry entry){ return entryToInput(entry).readByte(); }

	 public static byte entryToByte( DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToByte__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void byteToEntry__wrappee__base( byte val, DatabaseEntry entry){ outputToEntry(sizedOutput().writeByte(val),entry); }

	 public static void byteToEntry( byte val, DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	byteToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private static TupleOutput sizedOutput__wrappee__base(){ return new TupleOutput(new byte[BYTE_SIZE]); }

	 private static TupleOutput sizedOutput(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	sizedOutput__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
