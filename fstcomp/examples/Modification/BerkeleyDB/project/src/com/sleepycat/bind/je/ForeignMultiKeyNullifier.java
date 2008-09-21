package com.sleepycat.je; 
import de.ovgu.cide.jakutil.*; 
public  interface  ForeignMultiKeyNullifier {
	 public boolean nullifyForeignKey( SecondaryDatabase secondary, DatabaseEntry key, DatabaseEntry data, DatabaseEntry secKey) throws DatabaseException ;


}
