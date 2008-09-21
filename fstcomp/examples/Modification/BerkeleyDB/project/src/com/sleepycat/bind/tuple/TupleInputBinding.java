package com.sleepycat.bind.tuple; 
import com.sleepycat.bind.EntryBinding; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  class  TupleInputBinding  implements EntryBinding {
	 public TupleInputBinding(){ }

	 public Object entryToObject__wrappee__base( DatabaseEntry entry){ return TupleBinding.entryToInput(entry); }

	 public Object entryToObject( DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToObject__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void objectToEntry__wrappee__base( Object object, DatabaseEntry entry){ TupleBinding.inputToEntry((TupleInput)object,entry); }

	 public void objectToEntry( Object object, DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
