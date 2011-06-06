

package org.gjt.sp.jedit.buffer;

import java.util.*;
import javax.swing.text.Segment;
import org.gjt.sp.jedit.Buffer;
import org.gjt.sp.jedit.MiscUtilities;
import org.gjt.sp.jedit.ServiceManager;
import org.gjt.sp.util.Log;


public abstract class FoldHandler
{
	
	public static final String SERVICE = "org.gjt.sp.jedit.buffer.FoldHandler";

	
	
	public String getName()
	{
		return name;
	}
	

	
	
	public abstract int getFoldLevel(Buffer buffer, int lineIndex, Segment seg);
	

	
	
	public boolean equals(Object o)
	{
		
		if(o == null)
			return false;
		else
			return getClass() == o.getClass();
	} 

	
	public int hashCode()
	{
		return getClass().hashCode();
	} 

	
	
	public static void registerFoldHandler(FoldHandler handler)
	{
		if (getFoldHandler(handler.getName()) != null)
		{
			Log.log(Log.ERROR, FoldHandler.class, "Cannot register more than one fold handler with the same name");
			return;
		}

		foldHandlers.add(handler);
	}
	

	
	
	public static void unregisterFoldHandler(FoldHandler handler)
	{
		foldHandlers.remove(handler);
	}
	

	
	
	public static FoldHandler[] getFoldHandlers()
	{
		FoldHandler[] handlers = new FoldHandler[foldHandlers.size()];
		return (FoldHandler[])foldHandlers.toArray(handlers);
	}
	

	
	
	public static FoldHandler getFoldHandler(String name)
	{
		FoldHandler handler = (FoldHandler)ServiceManager
			.getService(SERVICE,name);
		if(handler != null)
			return handler;

		Iterator i = foldHandlers.iterator();
		while (i.hasNext())
		{
			handler = (FoldHandler)i.next();
			if (name.equals(handler.getName())) return handler;
		}

		return null;
	}
	

	
	
	public static String[] getFoldModes()
	{
		FoldHandler[] handlers = getFoldHandlers();
		String[] newApi = ServiceManager.getServiceNames(SERVICE);
		String[] foldModes = new String[handlers.length
			+ newApi.length];
		System.arraycopy(newApi,0,foldModes,0,newApi.length);

		for (int i = 0; i < handlers.length; i++)
		{
			foldModes[i + newApi.length] = handlers[i].getName();
		}

		Arrays.sort(foldModes,new MiscUtilities.StringCompare());
		return foldModes;
	}
	

	
	protected FoldHandler(String name)
	{
		this.name = name;
	}
	

	
	public String toString()
	{
		return name;
	} 

	
	private String name;

	
	private static ArrayList foldHandlers;
	

	
	static
	{
		foldHandlers = new ArrayList();
	}
	
}
