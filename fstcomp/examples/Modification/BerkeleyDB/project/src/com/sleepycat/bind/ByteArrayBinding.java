package com.sleepycat.bind; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  class  ByteArrayBinding  implements EntryBinding {
	 private static byte[] ZERO_LENGTH_BYTE_ARRAY=new byte[0];

	 public ByteArrayBinding(){ }

	 public Object entryToObject__wrappee__base( DatabaseEntry entry){ int len=entry.getSize(); if (len == 0) { return ZERO_LENGTH_BYTE_ARRAY; } else { byte[] bytes=new byte[len]; System.arraycopy(entry.getData(),entry.getOffset(),bytes,0,bytes.length); return bytes; } }

	 public Object entryToObject( DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToObject__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void objectToEntry__wrappee__base( Object object, DatabaseEntry entry){ byte[] bytes=(byte[])object; entry.setData(bytes,0,bytes.length); }

	 public void objectToEntry( Object object, DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
