

package org.gjt.sp.util;


public abstract class WorkRequest implements Runnable, ProgressObserver
{
	
	private boolean largeValues;

	
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

	
	
	public void setValue(long value)
	{
		Thread thread = Thread.currentThread();
		if(thread instanceof WorkThread)
		{
			if (largeValues)
			{
				((WorkThread)thread).setProgressValue((int) (value >> 10));
			}
			else
			{
				((WorkThread)thread).setProgressValue((int) value);
			}
		}
	} 

	
	
	public void setMaximum(long value)
	{
		Thread thread = Thread.currentThread();
		if(thread instanceof WorkThread)
		{
			if (value > Integer.MAX_VALUE)
			{
				largeValues = true;
				((WorkThread)thread).setProgressMaximum((int) (value >> 10));
			}
			else
			{
				largeValues = false;
				((WorkThread)thread).setProgressMaximum((int) value);
			}
		}
	} 
}
