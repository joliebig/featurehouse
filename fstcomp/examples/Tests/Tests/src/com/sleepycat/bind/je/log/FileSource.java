package com.sleepycat.je.log; 
import java.io.IOException; 
import java.io.RandomAccessFile; 
import java.nio.ByteBuffer; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import de.ovgu.cide.jakutil.*; 
 
class  FileSource  implements LogSource {
	 private RandomAccessFile file;

	 private int readBufferSize;

	 private FileManager fileManager;

	 FileSource( RandomAccessFile file, int readBufferSize, FileManager fileManager){ this.file=file; this.readBufferSize=readBufferSize; this.fileManager=fileManager; }

	 public void release() throws DatabaseException { }

	 public ByteBuffer getBytes( long fileOffset) throws IOException { ByteBuffer destBuf=ByteBuffer.allocate(readBufferSize); fileManager.readFromFile(file,destBuf,fileOffset); assert EnvironmentImpl.maybeForceYield(); destBuf.flip(); return destBuf; }

	 public ByteBuffer getBytes( long fileOffset, int numBytes) throws IOException { ByteBuffer destBuf=ByteBuffer.allocate(numBytes); fileManager.readFromFile(file,destBuf,fileOffset); assert EnvironmentImpl.maybeForceYield(); destBuf.flip(); assert destBuf.remaining() >= numBytes : "remaining=" + destBuf.remaining() + " numBytes="+ numBytes; return destBuf; }


}
