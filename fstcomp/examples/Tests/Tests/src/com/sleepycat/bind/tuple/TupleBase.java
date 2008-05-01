package com.sleepycat.bind.tuple; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  class  TupleBase {
	 private int outputBufferSize;

	 public TupleBase(){ outputBufferSize=0; }

	 public void setTupleBufferSize( int byteSize){ outputBufferSize=byteSize; }

	 public int getTupleBufferSize(){ return outputBufferSize; }

	 protected TupleOutput getTupleOutput( Object object){ int byteSize=getTupleBufferSize(); if (byteSize != 0) { return new TupleOutput(new byte[byteSize]); } else { return new TupleOutput(); } }

	 public static void outputToEntry( TupleOutput output, DatabaseEntry entry){ entry.setData(output.getBufferBytes(),output.getBufferOffset(),output.getBufferLength()); }

	 public static void inputToEntry( TupleInput input, DatabaseEntry entry){ entry.setData(input.getBufferBytes(),input.getBufferOffset(),input.getBufferLength()); }

	 public static TupleInput entryToInput( DatabaseEntry entry){ return new TupleInput(entry.getData(),entry.getOffset(),entry.getSize()); }

	 public static TupleOutput newOutput(){ return new TupleOutput(); }

	 public static TupleOutput newOutput( byte[] buffer){ return new TupleOutput(buffer); }


}
