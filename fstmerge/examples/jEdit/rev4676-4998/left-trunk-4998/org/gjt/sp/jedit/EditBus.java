

package org.gjt.sp.jedit;

import java.util.*;
import org.gjt.sp.util.Log;


public class EditBus
{
	
	
	public static void addToBus(EBComponent comp)
	{
		synchronized(components)
		{
			components.add(comp);
			copyComponents = null;
		}
	} 

	
	
	public static void removeFromBus(EBComponent comp)
	{
		synchronized(components)
		{
			components.remove(comp);
			copyComponents = null;
		}
	} 

	
	
	public static EBComponent[] getComponents()
	{
		synchronized(components)
		{
			if (copyComponents == null)
			{
				copyComponents = (EBComponent[])components.toArray(
					new EBComponent[components.size()]);
			}
			return copyComponents;
		}
	} 

	
	
	public static void send(EBMessage message)
	{
		Log.log(Log.DEBUG,EditBus.class,message.toString());

		
		
		EBComponent[] comps = getComponents();

		for(int i = 0; i < comps.length; i++)
		{
			try
			{
				EBComponent comp = comps[i];
				if(Debug.EB_TIMER)
				{
					long start = System.currentTimeMillis();
					comp.handleMessage(message);
					long time = (System.currentTimeMillis() - start);
					if(time != 0)
					{
						Log.log(Log.DEBUG,EditBus.class,comp + ": " + time + " ms");
					}
				}
				else
					comps[i].handleMessage(message);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,EditBus.class,"Exception"
					+ " while sending message on EditBus:");
				Log.log(Log.ERROR,EditBus.class,t);
			}
		}
	} 

	
	private static ArrayList components = new ArrayList();
	private static EBComponent[] copyComponents;

	
	private EditBus() {}
	
}
