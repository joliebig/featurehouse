package com.sleepycat.bind; 
import com.sleepycat.je.DatabaseEntry; 
import de.ovgu.cide.jakutil.*; 
public  interface  EntryBinding {
	 Object entryToObject( DatabaseEntry entry);

	 void objectToEntry( Object object, DatabaseEntry entry);


}
