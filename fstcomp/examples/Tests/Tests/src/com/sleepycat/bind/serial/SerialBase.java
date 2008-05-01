package com.sleepycat.bind.serial; 
import com.sleepycat.util.FastOutputStream; 
import de.ovgu.cide.jakutil.*; 
public  class  SerialBase {
	 private int outputBufferSize;

	 public SerialBase(){ outputBufferSize=0; }

	 public void setSerialBufferSize( int byteSize){ outputBufferSize=byteSize; }

	 public int getSerialBufferSize(){ return outputBufferSize; }

	 protected FastOutputStream getSerialOutput( Object object){ int byteSize=getSerialBufferSize(); if (byteSize != 0) { return new FastOutputStream(byteSize); } else { return new FastOutputStream(); } }


}
