package com.sleepycat.je; 
import java.util.Set; 
import de.ovgu.cide.jakutil.*; 
public  interface  SecondaryMultiKeyCreator {
	 public void createSecondaryKeys( SecondaryDatabase secondary, DatabaseEntry key, DatabaseEntry data, Set results) throws DatabaseException ;


}
