package com.sleepycat.je; 
import de.ovgu.cide.jakutil.*; 
public  interface  SecondaryKeyCreator {
	 public boolean createSecondaryKey( SecondaryDatabase secondary, DatabaseEntry key, DatabaseEntry data, DatabaseEntry result) throws DatabaseException ;


}
