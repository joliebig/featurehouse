package com.sleepycat.bind.tuple; 
import java.util.HashMap; 
import java.util.Map; 
import com.sleepycat.bind.EntryBinding; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public abstract  class  TupleBinding  extends TupleBase  implements EntryBinding {
	 private static final Map primitives=new HashMap();

	
static { addPrimitive(String.class,String.class,new StringBinding()); addPrimitive(Character.class,Character.TYPE,new CharacterBinding()); addPrimitive(Boolean.class,Boolean.TYPE,new BooleanBinding()); addPrimitive(Byte.class,Byte.TYPE,new ByteBinding()); addPrimitive(Short.class,Short.TYPE,new ShortBinding()); addPrimitive(Integer.class,Integer.TYPE,new IntegerBinding()); addPrimitive(Long.class,Long.TYPE,new LongBinding()); addPrimitive(Float.class,Float.TYPE,new FloatBinding()); addPrimitive(Double.class,Double.TYPE,new DoubleBinding()); }

	 public TupleBinding(){ }

	 private static void addPrimitive__wrappee__base( Class cls1, Class cls2, TupleBinding binding){ primitives.put(cls1,binding); primitives.put(cls2,binding); }

	 private static void addPrimitive( Class cls1, Class cls2, TupleBinding binding){ t.in(Thread.currentThread().getStackTrace()[1].toString());	addPrimitive__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Object entryToObject__wrappee__base( DatabaseEntry entry){ return entryToObject(entryToInput(entry)); }

	 public Object entryToObject( DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToObject__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void objectToEntry__wrappee__base( Object object, DatabaseEntry entry){ TupleOutput output=getTupleOutput(object); objectToEntry(object,output); outputToEntry(output,entry); }

	 public void objectToEntry( Object object, DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract Object entryToObject__wrappee__base( TupleInput input);

	 public abstract Object entryToObject( TupleInput input);{ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToObject__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public abstract void objectToEntry__wrappee__base( Object object, TupleOutput output);

	 public abstract void objectToEntry( Object object, TupleOutput output);{ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static TupleBinding getPrimitiveBinding__wrappee__base( Class cls){ return (TupleBinding)primitives.get(cls); }

	 public static TupleBinding getPrimitiveBinding( Class cls){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getPrimitiveBinding__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
