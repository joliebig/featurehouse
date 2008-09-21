package com.sleepycat.je.log; 
import java.nio.ByteBuffer; 
import de.ovgu.cide.jakutil.*; 
public  interface  LogWritable {
	 public int getLogSize();

	 public void writeToLog( ByteBuffer logBuffer);


}
