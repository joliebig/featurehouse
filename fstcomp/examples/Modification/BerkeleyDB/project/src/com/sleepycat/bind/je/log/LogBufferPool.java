package com.sleepycat.je.log; 
import java.io.IOException; 
import java.nio.ByteBuffer; 
import java.util.Iterator; 
import java.util.LinkedList; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.je.config.EnvironmentParams; 
import com.sleepycat.je.dbi.DbConfigManager; 
import com.sleepycat.je.dbi.EnvironmentImpl; 
import de.ovgu.cide.jakutil.*; 
 
class  LogBufferPool {
	 private static final String DEBUG_NAME=LogBufferPool.class.getName();

	 private EnvironmentImpl envImpl=null;

	 private int logBufferSize;

	 private LinkedList bufferPool;

	 private LogBuffer currentWriteBuffer;

	 private FileManager fileManager;

	 private boolean runInMemory;

	 LogBufferPool( FileManager fileManager, EnvironmentImpl envImpl) throws DatabaseException { this.fileManager=fileManager; this.envImpl=envImpl; this.hook485(envImpl); DbConfigManager configManager=envImpl.getConfigManager(); runInMemory=configManager.getBoolean(EnvironmentParams.LOG_MEMORY_ONLY); reset(configManager); currentWriteBuffer=(LogBuffer)bufferPool.getFirst(); }

	 void reset__wrappee__base( DbConfigManager configManager) throws DatabaseException { if (runInMemory && bufferPool != null) { return; } int numBuffers=configManager.getInt(EnvironmentParams.NUM_LOG_BUFFERS); long logBufferBudget=envImpl.getMemoryBudget().getLogBufferBudget(); int newBufferSize=(int)logBufferBudget / numBuffers; LinkedList newPool=new LinkedList(); if (runInMemory) { numBuffers=1; } for (int i=0; i < numBuffers; i++) { newPool.add(new LogBuffer(newBufferSize,envImpl)); } this.hook486(); bufferPool=newPool; logBufferSize=newBufferSize; }

	 void reset( DbConfigManager configManager) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	reset__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 LogBuffer getWriteBuffer__wrappee__base( int sizeNeeded, boolean flippedFile) throws IOException, DatabaseException { if ((!currentWriteBuffer.hasRoom(sizeNeeded)) || flippedFile) { writeBufferToFile(sizeNeeded); } if (flippedFile) { if (!runInMemory) { fileManager.syncLogEndAndFinishFile(); } } return currentWriteBuffer; }

	 LogBuffer getWriteBuffer( int sizeNeeded, boolean flippedFile) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getWriteBuffer__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void writeBufferToFile__wrappee__base( int sizeNeeded) throws IOException, DatabaseException { int bufferSize=((logBufferSize > sizeNeeded) ? logBufferSize : sizeNeeded); this.hook488(); LogBuffer latchedBuffer=currentWriteBuffer; this.hook487(bufferSize,latchedBuffer); }

	 void writeBufferToFile( int sizeNeeded) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	writeBufferToFile__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 void writeCompleted__wrappee__base( long lsn, boolean flushRequired) throws DatabaseException, IOException { currentWriteBuffer.registerLsn(lsn); if (flushRequired) { writeBufferToFile(0); } }

	 void writeCompleted( long lsn, boolean flushRequired) throws DatabaseException, IOException { t.in(Thread.currentThread().getStackTrace()[1].toString());	writeCompleted__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 LogBuffer getReadBuffer__wrappee__base( long lsn) throws DatabaseException { LogBuffer foundBuffer=null; foundBuffer=this.hook489(lsn,foundBuffer); if (foundBuffer == null) { return null; } else { return foundBuffer; } }

	 LogBuffer getReadBuffer( long lsn) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	getReadBuffer__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook485__wrappee__base( EnvironmentImpl envImpl) throws DatabaseException { }

	 protected void hook485( EnvironmentImpl envImpl) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook485__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook486__wrappee__base() throws DatabaseException { }

	 protected void hook486() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook486__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook487__wrappee__base( int bufferSize, LogBuffer latchedBuffer) throws IOException, DatabaseException { ByteBuffer currentByteBuffer=currentWriteBuffer.getDataBuffer(); int savePosition=currentByteBuffer.position(); int saveLimit=currentByteBuffer.limit(); currentByteBuffer.flip(); if (runInMemory) { this.hook492(latchedBuffer); latchedBuffer=null; this.hook491(); currentWriteBuffer=new LogBuffer(bufferSize,envImpl); bufferPool.add(currentWriteBuffer); this.hook490(); } else { try { fileManager.writeLogBuffer(currentWriteBuffer); currentWriteBuffer.getDataBuffer().rewind(); this.hook494(latchedBuffer); latchedBuffer=null; LogBuffer nextToUse=null; this.hook493(nextToUse); } catch ( DatabaseException DE) { currentByteBuffer.position(savePosition); currentByteBuffer.limit(saveLimit); throw DE; } } }

	 protected void hook487( int bufferSize, LogBuffer latchedBuffer) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook487__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook488__wrappee__base() throws IOException, DatabaseException { }

	 protected void hook488() throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook488__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected LogBuffer hook489__wrappee__base( long lsn, LogBuffer foundBuffer) throws DatabaseException { Iterator iter=bufferPool.iterator(); while (iter.hasNext()) { LogBuffer l=(LogBuffer)iter.next(); if (l.containsLsn(lsn)) { foundBuffer=l; break; } } if (foundBuffer == null && currentWriteBuffer.containsLsn(lsn)) { foundBuffer=currentWriteBuffer; } if (foundBuffer == null) { this.hook496(); } return foundBuffer; }

	 protected LogBuffer hook489( long lsn, LogBuffer foundBuffer) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook489__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook490__wrappee__base() throws IOException, DatabaseException { }

	 protected void hook490() throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook490__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook491__wrappee__base() throws IOException, DatabaseException { }

	 protected void hook491() throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook491__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook492__wrappee__base( LogBuffer latchedBuffer) throws IOException, DatabaseException { }

	 protected void hook492( LogBuffer latchedBuffer) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook492__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook493__wrappee__base( LogBuffer nextToUse) throws IOException, DatabaseException { this.hook495(); Iterator iter=bufferPool.iterator(); nextToUse=(LogBuffer)iter.next(); boolean done=bufferPool.remove(nextToUse); assert done; nextToUse.reinit(); bufferPool.add(nextToUse); currentWriteBuffer=nextToUse; }

	 protected void hook493( LogBuffer nextToUse) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook493__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook494__wrappee__base( LogBuffer latchedBuffer) throws IOException, DatabaseException { }

	 protected void hook494( LogBuffer latchedBuffer) throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook494__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook495__wrappee__base() throws IOException, DatabaseException { }

	 protected void hook495() throws IOException, DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook495__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected void hook496__wrappee__base() throws DatabaseException { }

	 protected void hook496() throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	hook496__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
