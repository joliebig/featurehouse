package com.sleepycat.je.utilint; 
import java.io.IOException; 
import de.ovgu.cide.jakutil.*; 
public  interface  TestHook {
	 public void doIOHook() throws IOException ;

	 public void doHook();

	 public Object getHookValue();


}
