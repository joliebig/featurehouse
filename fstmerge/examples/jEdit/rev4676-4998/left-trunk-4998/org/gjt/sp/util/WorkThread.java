

package org.gjt.sp.util;


public class WorkThread extends Thread
{
	public WorkThread(WorkThreadPool pool, ThreadGroup group, String name)
	{
		super(group, name);
		
		
		setPriority(Thread.MIN_PRIORITY);

		this.pool = pool;
	}

	
	public void setAbortable(boolean abortable)
	{
		synchronized(abortLock)
		{
			this.abortable = abortable;
			if(aborted)
				stop(new Abort());
		}
	}

	
	public boolean isRequestRunning()
	{
		return requestRunning;
	}

	
	public String getStatus()
	{
		return status;
	}

	
	public void setStatus(String status)
	{
		this.status = status;
		pool.fireProgressChanged(this);
	}

	
	public int getProgressValue()
	{
		return progressValue;
	}

	
	public void setProgressValue(int progressValue)
	{
		this.progressValue = progressValue;
		pool.fireProgressChanged(this);
	}

	
	public int getProgressMaximum()
	{
		return progressMaximum;
	}

	
	public void setProgressMaximum(int progressMaximum)
	{
		this.progressMaximum = progressMaximum;
		pool.fireProgressChanged(this);
	}

	
	public void abortCurrentRequest()
	{
		synchronized(abortLock)
		{
			if(abortable && !aborted)
				stop(new Abort());
			aborted = true;
		}
	}

	public void run()
	{
		Log.log(Log.DEBUG,this,"Work request thread starting [" + getName() + "]");

		for(;;)
		{
			doRequests();
		}
	}

	
	private WorkThreadPool pool;
	private Object abortLock = new Object();
	private boolean requestRunning;
	private boolean abortable;
	private boolean aborted;
	private String status;
	private int progressValue;
	private int progressMaximum;

	private void doRequests()
	{
		WorkThreadPool.Request request;
		for(;;)
		{
			request = pool.getNextRequest();
			if(request == null)
				break;
			else
			{
				requestRunning = true;
				pool.fireStatusChanged(this);
				doRequest(request);
				requestRunning = false;
			}
		}

		pool.fireStatusChanged(this);

		synchronized(pool.waitForAllLock)
		{
			
			pool.waitForAllLock.notifyAll();
		}

		synchronized(pool.lock)
		{
			
			try
			{
				pool.lock.wait();
			}
			catch(InterruptedException ie)
			{
				Log.log(Log.ERROR,this,ie);
			}
		}
	}

	private void doRequest(WorkThreadPool.Request request)
	{
		Log.log(Log.DEBUG,WorkThread.class,"Running in work thread: " + request);

		try
		{
			request.run.run();
		}
		catch(Abort a)
		{
			Log.log(Log.ERROR,WorkThread.class,"Unhandled abort");
		}
		catch(Throwable t)
		{
			Log.log(Log.ERROR,WorkThread.class,"Exception "
				+ "in work thread:");
			Log.log(Log.ERROR,WorkThread.class,t);
		}
		finally
		{
			synchronized(abortLock)
			{
				aborted = abortable = false;
			}
			status = null;
			progressValue = progressMaximum = 0;
			pool.requestDone();
			pool.fireStatusChanged(this);
		}
	}

	public static class Abort extends Error
	{
		public Abort()
		{
			super("Work request aborted");
		}
	}
}
