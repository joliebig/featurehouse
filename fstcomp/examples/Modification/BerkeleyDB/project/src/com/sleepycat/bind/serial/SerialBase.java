package com.sleepycat.bind.serial; 
import com.sleepycat.util.FastOutputStream; 
import de.ovgu.cide.jakutil.*; 
public  class  SerialBase {
	 private int outputBufferSize;

	 public SerialBase(){ outputBufferSize=0; }

	 public void setSerialBufferSize__wrappee__base( int byteSize){ outputBufferSize=byteSize; }

	 public void setSerialBufferSize( int byteSize){ t.in(Thread.currentThread().getStackTrace()[1].toString());	setSerialBufferSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public int getSerialBufferSize__wrappee__base(){ return outputBufferSize; }

	 public int getSerialBufferSize(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getSerialBufferSize__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected FastOutputStream getSerialOutput__wrappee__base( Object object){ int byteSize=getSerialBufferSize(); if (byteSize != 0) { return new FastOutputStream(byteSize); } else { return new FastOutputStream(); } }

	 protected FastOutputStream getSerialOutput( Object object){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getSerialOutput__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
