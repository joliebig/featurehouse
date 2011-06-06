

package org.gjt.sp.jedit.buffer;

import java.util.*;
import javax.swing.text.Segment;
import org.gjt.sp.jedit.ServiceManager;
import org.gjt.sp.util.StandardUtilities;


public abstract class FoldHandler
{
	
	public static final String SERVICE = "org.gjt.sp.jedit.buffer.FoldHandler";

	
	
	public String getName()
	{
		return name;
	}
	

	
	
	public abstract int getFoldLevel(JEditBuffer buffer, int lineIndex, Segment seg);
	

	
	
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

	
	
	public static FoldHandler getFoldHandler(String name)
	{
		FoldHandler handler = (FoldHandler)ServiceManager
			.getService(SERVICE,name);
		return handler;
	}
	

	
	
	public static String[] getFoldModes()
	{
		String[] handlers = ServiceManager.getServiceNames(SERVICE);
		Arrays.sort(handlers,new StandardUtilities.StringCompare());
		return handlers;
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
}
