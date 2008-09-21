package com.sleepycat.je.log; 
import java.nio.ByteBuffer; 
import de.ovgu.cide.jakutil.*; 
public  interface  LogReadable {
	 public void readFromLog( ByteBuffer itemBuffer, byte entryTypeVersion) throws LogException ;

	 public void dumpLog( StringBuffer sb, boolean verbose);

	 public boolean logEntryIsTransactional();

	 public long getTransactionId();


}
