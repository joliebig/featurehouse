

package org.gjt.sp.util;


public abstract class WorkRequest implements Runnable
{
	
	public void setAbortable(boolean abortable)
	{
		Thread thread = Thread.currentThread();
		if(thread instanceof WorkThread)
			((WorkThread)thread).setAbortable(abortable);
	}

	
	public void setStatus(String status)
	{
		Thread thread = Thread.currentThread();
		if(thread instanceof WorkThread)
			((WorkThread)thread).setStatus(status);
	}

	
	public void setProgressValue(int value)
	{
		Thread thread = Thread.currentThread();
		if(thread instanceof WorkThread)
			((WorkThread)thread).setProgressValue(value);
	}

	
	public void setProgressMaximum(int value)
	{
		Thread thread = Thread.currentThread();
		if(thread instanceof WorkThread)
			((WorkThread)thread).setProgressMaximum(value);
	}
}
