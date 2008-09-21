package com.sleepycat.bind.tuple; 
import de.ovgu.cide.jakutil.*; 
public  class  TupleTupleMarshalledKeyCreator  extends TupleTupleKeyCreator {
	 private String keyName;

	 private TupleTupleMarshalledBinding binding;

	 public TupleTupleMarshalledKeyCreator( TupleTupleMarshalledBinding binding, String keyName){ this.binding=binding; this.keyName=keyName; }

	 public boolean createSecondaryKey__wrappee__base( TupleInput primaryKeyInput, TupleInput dataInput, TupleOutput indexKeyOutput){ MarshalledTupleKeyEntity entity=(MarshalledTupleKeyEntity)binding.entryToObject(primaryKeyInput,dataInput); return entity.marshalSecondaryKey(keyName,indexKeyOutput); }

	 public boolean createSecondaryKey( TupleInput primaryKeyInput, TupleInput dataInput, TupleOutput indexKeyOutput){ t.in(Thread.currentThread().getStackTrace()[1].toString());	createSecondaryKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public boolean nullifyForeignKey__wrappee__base( TupleInput dataInput, TupleOutput dataOutput){ MarshalledTupleKeyEntity entity=(MarshalledTupleKeyEntity)binding.entryToObject(null,dataInput); if (entity.nullifyForeignKey(keyName)) { binding.objectToData(entity,dataOutput); return true; } else { return false; } }

	 public boolean nullifyForeignKey( TupleInput dataInput, TupleOutput dataOutput){ t.in(Thread.currentThread().getStackTrace()[1].toString());	nullifyForeignKey__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
