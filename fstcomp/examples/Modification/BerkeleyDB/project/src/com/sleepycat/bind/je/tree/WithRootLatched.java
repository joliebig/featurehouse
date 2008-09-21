package com.sleepycat.je.tree; 
import com.sleepycat.je.DatabaseException; 
import de.ovgu.cide.jakutil.*; 
public  interface  WithRootLatched {
	 public IN doWork( ChildReference root) throws DatabaseException ;


}
