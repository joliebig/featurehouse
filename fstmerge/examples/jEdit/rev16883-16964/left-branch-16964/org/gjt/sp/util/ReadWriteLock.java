

package org.gjt.sp.util;

import java.util.concurrent.locks.ReentrantReadWriteLock;


public class ReadWriteLock
{
	
	public void readLock()
	{
		body.readLock().lock();
	} 

	
	public void readUnlock()
	{
		body.readLock().unlock();
	} 

	
	public void writeLock()
	{
		body.writeLock().lock();
	} 

	
	public void writeUnlock()
	{
		body.writeLock().unlock();
	} 

	
	public boolean isWriteLocked()
	{
		return body.isWriteLocked();
	} 

	
	private final ReentrantReadWriteLock body = new ReentrantReadWriteLock();
	
}
