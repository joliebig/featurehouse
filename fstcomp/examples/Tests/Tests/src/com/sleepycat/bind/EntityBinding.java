package com.sleepycat.bind; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  interface  EntityBinding {
	 Object entryToObject( DatabaseEntry key, DatabaseEntry data);

	 void objectToKey( Object object, DatabaseEntry key);

	 void objectToData( Object object, DatabaseEntry data);


}
