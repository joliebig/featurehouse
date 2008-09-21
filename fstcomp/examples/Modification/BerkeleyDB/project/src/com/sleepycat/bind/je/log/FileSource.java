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

	 public void release__wrappee__base() throws DatabaseException { }

	 public void release() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	release__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public ByteBuffer getBytes__wrappee__base( long fileOffset) throws IOException { ByteBuffer destBuf=ByteBuffer.allocate(readBufferSize); fileManager.readFromFile(file,destBuf,fileOffset); assert EnvironmentImpl.maybeForceYield(); destBuf.flip(); return destBuf; }

	 public ByteBuffer getBytes( long fileOffset) throws IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getBytes__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public ByteBuffer getBytes__wrappee__base( long fileOffset, int numBytes) throws IOException { ByteBuffer destBuf=ByteBuffer.allocate(numBytes); fileManager.readFromFile(file,destBuf,fileOffset); assert EnvironmentImpl.maybeForceYield(); destBuf.flip(); assert destBuf.remaining() >= numBytes : "remaining=" + destBuf.remaining() + " numBytes="+ numBytes; return destBuf; }

	 public ByteBuffer getBytes( long fileOffset, int numBytes) throws IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getBytes__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
