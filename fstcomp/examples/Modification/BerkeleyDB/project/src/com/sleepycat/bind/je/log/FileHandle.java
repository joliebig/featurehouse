package com.sleepycat.je.log; 
import java.io.IOException; 
import java.io.RandomAccessFile; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import de.ovgu.cide.jakutil.*; 
 
class  FileHandle {
	 private RandomAccessFile file;

	 private boolean oldHeaderVersion;

	 FileHandle( RandomAccessFile file, String fileName, EnvironmentImpl env, boolean oldHeaderVersion){ this.file=file; this.oldHeaderVersion=oldHeaderVersion; this.hook444(fileName,env); }

	 RandomAccessFile getFile__wrappee__base(){ return file; }

	 RandomAccessFile getFile(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getFile__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 boolean isOldHeaderVersion__wrappee__base(){ return oldHeaderVersion; }

	 boolean isOldHeaderVersion(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	isOldHeaderVersion__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void close__wrappee__base() throws IOException { if (file != null) { file.close(); file=null; } }

	 void close() throws IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	close__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook444__wrappee__base( String fileName, EnvironmentImpl env){ }

	 protected void hook444( String fileName, EnvironmentImpl env){ t.in(Thread.currentThread().getStackTrace()[1].toString());	hook444__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
