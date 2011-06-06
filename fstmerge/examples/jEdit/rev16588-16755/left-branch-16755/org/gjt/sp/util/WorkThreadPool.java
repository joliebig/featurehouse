

package org.gjt.sp.util;


import javax.swing.event.EventListenerList;
import javax.swing.SwingUtilities;



public class WorkThreadPool
{
	
	
	public WorkThreadPool(String name, int count)
	{
		listenerList = new EventListenerList();

		if(count != 0)
		{
			threadGroup = new ThreadGroup(name);
			threads = new WorkThread[count];
			for(int i = 0; i < threads.length; i++)
			{
				threads[i] = new WorkThread(this,threadGroup,name + " #" + (i+1));
			}
		}
		else
			Log.log(Log.WARNING,this,"Async I/O disabled");
	} 

	
	
	public void start()
	{
		
		synchronized(lock)
		{
			started = true;

			if(awtRequestCount != 0 && requestCount == 0)
				queueAWTRunner();
		}

		if(threads != null)
		{
			for(int i = 0; i < threads.length; i++)
			{
				threads[i].start();
			}
		}
	} 

	
	
	public void addWorkRequest(Runnable run, boolean inAWT)
	{
		if(threads == null)
		{
			run.run();
			return;
		}

		synchronized(lock)
		{
			
			if(started && inAWT && requestCount == 0 && awtRequestCount == 0)
			{


				if(SwingUtilities.isEventDispatchThread())
					run.run();
				else
					SwingUtilities.invokeLater(run);

				return;
			} 

			Request request = new Request(run);

			
			if(inAWT)
			{
				if(firstAWTRequest == null && lastAWTRequest == null)
					firstAWTRequest = lastAWTRequest = request;
				else
				{
					lastAWTRequest.next = request;
					lastAWTRequest = request;
				}

				awtRequestCount++;

				
				
				
				if(started && requestCount == 0)
					queueAWTRunner();
			} 
			
			else
			{
				if(firstRequest == null && lastRequest == null)
					firstRequest = lastRequest = request;
				else
				{
					lastRequest.next = request;
					lastRequest = request;
				}

				requestCount++;
			} 

			lock.notifyAll();
		}
	} 

	
	
	public void waitForRequests()
	{
		if(threads == null)
			return;

		synchronized(waitForAllLock)
		{
			while(requestCount != 0)
			{
				try
				{
					waitForAllLock.wait();
				}
				catch(InterruptedException ie)
				{
					Log.log(Log.ERROR,this,ie);
				}
			}
		}

		if(SwingUtilities.isEventDispatchThread())
		{
			
			doAWTRequests();
		}
		else
		{
			try
			{
				SwingUtilities.invokeAndWait(new RunRequestsInAWTThread());
			}
			catch(Exception e)
			{
				Log.log(Log.ERROR,this,e);
			}
		}
	} 

	
	
	public int getRequestCount()
	{
		return requestCount;
	} 

	
	
	public int getThreadCount()
	{
		if(threads == null)
			return 0;
		else
			return threads.length;
	} 

	
	
	public WorkThread getThread(int index)
	{
		return threads[index];
	} 

	
	
	public void addProgressListener(WorkThreadProgressListener listener)
	{
		listenerList.add(WorkThreadProgressListener.class,listener);
	} 

	
	
	public void removeProgressListener(WorkThreadProgressListener listener)
	{
		listenerList.remove(WorkThreadProgressListener.class,listener);
	} 

	
	final Object lock = new Object();
	final Object waitForAllLock = new Object();

	
	void fireStatusChanged(WorkThread thread)
	{
		final Object[] listeners = listenerList.getListenerList();
		if(listeners.length != 0)
		{
			int index = 0;
			for(int i = 0; i < threads.length; i++)
			{
				if(threads[i] == thread)
				{
					index = i;
					break;
				}
			}

			for(int i = listeners.length - 2; i >= 0; i--)
			{
				if(listeners[i] == WorkThreadProgressListener.class)
				{
					((WorkThreadProgressListener)listeners[i+1])
						.statusUpdate(WorkThreadPool.this,index);
				}
			}
		}
	} 

	
	void fireProgressChanged(WorkThread thread)
	{
		final Object[] listeners = listenerList.getListenerList();
		if(listeners.length != 0)
		{
			int index = 0;
			for(int i = 0; i < threads.length; i++)
			{
				if(threads[i] == thread)
				{
					index = i;
					break;
				}
			}

			for(int i = listeners.length - 2; i >= 0; i--)
			{
				if(listeners[i] == WorkThreadProgressListener.class)
				{
					((WorkThreadProgressListener)listeners[i+1])
						.progressUpdate(WorkThreadPool.this,index);
				}
			}
		}
	} 

	
	void requestDone()
	{
		synchronized(lock)
		{
			requestCount--;

			if(requestCount == 0 && firstAWTRequest != null)
				queueAWTRunner();
		}
	} 

	
	Request getNextRequest()
	{
		synchronized(lock)
		{
			Request request = firstRequest;
			if(request == null)
				return null;

			firstRequest = firstRequest.next;
			if(firstRequest == null)
				lastRequest = null;

			if(request.alreadyRun)
				throw new InternalError("AIEE!!! Request run twice!!! " + request.run);
			request.alreadyRun = true;

			

			return request;
		}
	} 

	

	

	
	private boolean started;
	private ThreadGroup threadGroup;
	private WorkThread[] threads;

	
	private Request firstRequest;
	private Request lastRequest;
	private int requestCount;

	
	private boolean awtRunnerQueued;
	private Request firstAWTRequest;
	private Request lastAWTRequest;
	private int awtRequestCount;

	private EventListenerList listenerList;
	

	
	
	private void doAWTRequests()
	{
		while(requestCount == 0 && firstAWTRequest != null)
		{
			doAWTRequest(getNextAWTRequest());
		}
	} 

	
	
	private void doAWTRequest(Request request)
	{


		try
		{
			request.run.run();
		}
		catch(Throwable t)
		{
			Log.log(Log.ERROR,WorkThread.class,"Exception "
				+ "in AWT thread:");
			Log.log(Log.ERROR,WorkThread.class,t);
		}

		awtRequestCount--;
	} 

	
	
	private void queueAWTRunner()
	{
		if(!awtRunnerQueued)
		{
			awtRunnerQueued = true;
			SwingUtilities.invokeLater(new RunRequestsInAWTThread());

		}
	} 

	
	private Request getNextAWTRequest()
	{
		Request request = firstAWTRequest;
		firstAWTRequest = firstAWTRequest.next;
		if(firstAWTRequest == null)
			lastAWTRequest = null;

		if(request.alreadyRun)
			throw new InternalError("AIEE!!! Request run twice!!! " + request.run);
		request.alreadyRun = true;

		

		return request;
	} 

	

	static int ID;

	
	static class Request
	{
		int id = ++ID;

		Runnable run;

		boolean alreadyRun;

		Request next;

		Request(Runnable run)
		{
			this.run = run;
		}

		public String toString()
		{
			return "[id=" + id + ",run=" + run + "]";
		}
	} 

	
	class RunRequestsInAWTThread implements Runnable
	{
		public void run()
		{
			synchronized(lock)
			{
				awtRunnerQueued = false;
				if(requestCount == 0)
					doAWTRequests();
			}
		}
	} 
}
