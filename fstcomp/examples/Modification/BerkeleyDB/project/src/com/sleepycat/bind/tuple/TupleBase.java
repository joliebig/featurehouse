package com.sleepycat.bind.tuple; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  class  TupleBase {
	 private int outputBufferSize;

	 public TupleBase(){ outputBufferSize=0; }

	 public void setTupleBufferSize__wrappee__base( int byteSize){ outputBufferSize=byteSize; }

	 public void setTupleBufferSize( int byteSize){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setTupleBufferSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getTupleBufferSize__wrappee__base(){ return outputBufferSize; }

	 public int getTupleBufferSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTupleBufferSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected TupleOutput getTupleOutput__wrappee__base( Object object){ int byteSize=getTupleBufferSize(); if (byteSize != 0) { return new TupleOutput(new byte[byteSize]); } else { return new TupleOutput(); } }

	 protected TupleOutput getTupleOutput( Object object){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getTupleOutput__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void outputToEntry__wrappee__base( TupleOutput output, DatabaseEntry entry){ entry.setData(output.getBufferBytes(),output.getBufferOffset(),output.getBufferLength()); }

	 public static void outputToEntry( TupleOutput output, DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	outputToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void inputToEntry__wrappee__base( TupleInput input, DatabaseEntry entry){ entry.setData(input.getBufferBytes(),input.getBufferOffset(),input.getBufferLength()); }

	 public static void inputToEntry( TupleInput input, DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	inputToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static TupleInput entryToInput__wrappee__base( DatabaseEntry entry){ return new TupleInput(entry.getData(),entry.getOffset(),entry.getSize()); }

	 public static TupleInput entryToInput( DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToInput__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static TupleOutput newOutput__wrappee__base(){ return new TupleOutput(); }

	 public static TupleOutput newOutput(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	newOutput__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static TupleOutput newOutput__wrappee__base( byte[] buffer){ return new TupleOutput(buffer); }

	 public static TupleOutput newOutput( byte[] buffer){ t.in(Thread.currentThread().getStackTrace()[1].toString());	newOutput__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
