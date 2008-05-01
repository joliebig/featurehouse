package com.sleepycat.je; 
import de.ovgu.cide.jakutil.*; 
public  interface  ForeignKeyNullifier {
	 public boolean nullifyForeignKey( SecondaryDatabase secondary, DatabaseEntry data) throws DatabaseException ;


}
