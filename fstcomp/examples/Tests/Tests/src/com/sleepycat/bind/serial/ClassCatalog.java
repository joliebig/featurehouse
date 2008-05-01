package com.sleepycat.bind.serial; 
import java.io.ObjectStreamClass; 
import com.sleepycat.je.DatabaseException; 
import de.ovgu.cide.jakutil.*; 
public  interface  ClassCatalog {
	 public void close() throws DatabaseException ;

	 public byte[] getClassID( ObjectStreamClass classDesc) throws DatabaseException, ClassNotFoundException ;

	 public ObjectStreamClass getClassFormat( byte[] classID) throws DatabaseException, ClassNotFoundException ;


}
