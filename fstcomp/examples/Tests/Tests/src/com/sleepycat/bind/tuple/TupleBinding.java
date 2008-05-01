package com.sleepycat.bind.tuple; 
import java.util.HashMap; 
import java.util.Map; 
import com.sleepycat.bind.EntryBinding; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public abstract  class  TupleBinding  extends TupleBase  implements EntryBinding {
	 private static final Map primitives=new HashMap();

	
static { addPrimitive(String.class,String.class,new StringBinding()); addPrimitive(Character.class,Character.TYPE,new CharacterBinding()); addPrimitive(Boolean.class,Boolean.TYPE,new BooleanBinding()); addPrimitive(Byte.class,Byte.TYPE,new ByteBinding()); addPrimitive(Short.class,Short.TYPE,new ShortBinding()); addPrimitive(Integer.class,Integer.TYPE,new IntegerBinding()); addPrimitive(Long.class,Long.TYPE,new LongBinding()); addPrimitive(Float.class,Float.TYPE,new FloatBinding()); addPrimitive(Double.class,Double.TYPE,new DoubleBinding()); }

	 private static void addPrimitive( Class cls1, Class cls2, TupleBinding binding){ primitives.put(cls1,binding); primitives.put(cls2,binding); }

	 public TupleBinding(){ }

	 public Object entryToObject( DatabaseEntry entry){ return entryToObject(entryToInput(entry)); }

	 public void objectToEntry( Object object, DatabaseEntry entry){ TupleOutput output=getTupleOutput(object); objectToEntry(object,output); outputToEntry(output,entry); }

	 public abstract Object entryToObject( TupleInput input);

	 public abstract void objectToEntry( Object object, TupleOutput output);

	 public static TupleBinding getPrimitiveBinding( Class cls){ return (TupleBinding)primitives.get(cls); }


}
