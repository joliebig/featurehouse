package com.sleepycat.je.log; 
import com.sleepycat.je.DatabaseException; 
import de.ovgu.cide.jakutil.*; 
 
class  FileHandleSource  extends FileSource {
	 private FileHandle fileHandle;

	 FileHandleSource( FileHandle fileHandle, int readBufferSize, FileManager fileManager){ super(fileHandle.getFile(),readBufferSize,fileManager); this.fileHandle=fileHandle; }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
