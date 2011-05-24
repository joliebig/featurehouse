package com.sleepycat.je.log; 
import java.io.IOException; 
import java.nio.ByteBuffer; 
import com.sleepycat.je.DatabaseException; 
import de.ovgu.cide.jakutil.*; 
 
interface  LogSource {
	 void release() throws DatabaseException ;

	 ByteBuffer getBytes( long fileOffset) throws IOException ;

	 ByteBuffer getBytes( long fileOffset, int numBytes) throws IOException ;


}
