package com.sleepycat.bind.tuple; 
import com.sleepycat.je.DatabaseEntry; 
import com.sleepycat.util.UtfOps; 
import de.ovgu.cide.jakutil.*; 
public  class  StringBinding  extends TupleBinding {
	 public Object entryToObject__wrappee__base( TupleInput input){ return input.readString(); }

	 public Object entryToObject( TupleInput input){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToObject__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void objectToEntry__wrappee__base( Object object, TupleOutput output){ output.writeString((String)object); }

	 public void objectToEntry( Object object, TupleOutput output){ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected TupleOutput getTupleOutput__wrappee__base( Object object){ return sizedOutput((String)object); }

	 protected TupleOutput getTupleOutput( Object object){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTupleOutput__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static String entryToString__wrappee__base( DatabaseEntry entry){ return entryToInput(entry).readString(); }

	 public static String entryToString( DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void stringToEntry__wrappee__base( String val, DatabaseEntry entry){ outputToEntry(sizedOutput(val).writeString(val),entry); }

	 public static void stringToEntry( String val, DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	stringToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 private static TupleOutput sizedOutput__wrappee__base( String val){ int stringLength=(val == null) ? 1 : UtfOps.getByteLength(val.toCharArray()); stringLength++; return new TupleOutput(new byte[stringLength]); }

	 private static TupleOutput sizedOutput( String val){ t.in(Thread.currentThread().getStackTrace()[1].toString());	sizedOutput__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
