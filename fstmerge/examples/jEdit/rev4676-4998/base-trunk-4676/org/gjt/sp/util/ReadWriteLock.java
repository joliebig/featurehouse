

package org.gjt.sp.util;

import java.util.Vector;


public class ReadWriteLock
{
	
	public synchronized void readLock()
	{
		
		
		if (activeReaders != 0 || allowRead())
		{
			++activeReaders;
			
			return;
		}
		++waitingReaders;
		while (!allowRead())
		{
			try
			{
				wait();
			}
			catch (InterruptedException e)
			{
				--waitingReaders; 
				Log.log(Log.ERROR,this,e);
				return;
			}
		}
		--waitingReaders;
		++activeReaders;
		readers.addElement(Thread.currentThread());
	} 

	
	public synchronized void readUnlock()
	{
		if(activeReaders == 0)
			throw new InternalError("Unbalanced readLock()/readUnlock() calls");

		--activeReaders;
		
		notifyAll();
	} 

	
	public synchronized void writeLock()
	{
		if (writerThread != null)
		{
			
			if (Thread.currentThread() == writerThread)
			{
				
				++lockCount;
				return;
			}
		}
		if (allowWrite())
		{
			claimWriteLock();
			return;
		}

		++waitingWriters;
		while (!allowWrite())
		{
			try
			{
				wait();
			}
			catch (InterruptedException e)
			{
				--waitingWriters;
				Log.log(Log.ERROR,this,e);
				return;
			}
		}
		--waitingWriters;
		claimWriteLock();
	} 

	
	public synchronized void writeUnlock()
	{
		if(activeWriters != 1 || lockCount <= 0)
			throw new InternalError("Unbalanced writeLock()/writeUnlock() calls");

		if(Thread.currentThread() != writerThread)
			throw new InternalError("writeUnlock() from wrong thread");

		if (--lockCount == 0)
		{
			--activeWriters;
			writerThread = null;
			notifyAll();
		}
	} 

	
	public synchronized boolean isWriteLocked()
	{
		
		return activeWriters == 1;
	} 

	

	
	private int activeReaders;
	private int activeWriters;
	private int waitingReaders;
	private int waitingWriters;
	private Vector readers = new Vector();

	private Thread writerThread;
	private int lockCount;
	

	
	private final boolean allowRead()
	{
		return (Thread.currentThread() == writerThread)
			|| (waitingWriters == 0 && activeWriters == 0);
	} 

	
	private final boolean allowWrite()
	{
		

		return activeReaders == 0 && activeWriters == 0;
	} 

	
	private void claimWriteLock()
	{
		++activeWriters;
		
		writerThread = Thread.currentThread();
		
		lockCount = 1;
	} 

	
}
