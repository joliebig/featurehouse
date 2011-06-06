

package org.gjt.sp.jedit;

import java.io.*;
import java.net.URL;
import java.util.*;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.XMLUtilities;


public class ServiceManager
{
	
	
	public static void loadServices(PluginJAR plugin, URL uri,
		PluginJAR.PluginCacheEntry cache)
	{
		ServiceListHandler dh = new ServiceListHandler(plugin,uri);
		try
		{
			if (!XMLUtilities.parseXML(uri.openStream(), dh)
				&& cache != null)
			{
				cache.cachedServices = dh.getCachedServices();
			}
		}
		catch (IOException ioe)
		{
			Log.log(Log.ERROR, ServiceManager.class, ioe);
		}
	} 

	
	
	public static void unloadServices(PluginJAR plugin)
	{
		Iterator<Descriptor> descriptors = serviceMap.keySet().iterator();
		while(descriptors.hasNext())
		{
			Descriptor d = descriptors.next();
			if(d.plugin == plugin)
				descriptors.remove();
		}
	} 

	
	
	public static void registerService(String clazz, String name,
		String code, PluginJAR plugin)
	{
		Descriptor d = new Descriptor(clazz,name,code,plugin);
		serviceMap.put(d,d);
	} 

	
	
	public static void unregisterService(String clazz, String name)
	{
		Descriptor d = new Descriptor(clazz,name);
		serviceMap.remove(d);
	} 

	
	
	public static String[] getServiceTypes()
	{
		Set<String> returnValue = new HashSet<String>();

		Set<Descriptor> keySet = serviceMap.keySet();
		for (Descriptor d : keySet)
			returnValue.add(d.clazz);

		return returnValue.toArray(
			new String[returnValue.size()]);
	} 

	
	
	public static String[] getServiceNames(String clazz)
	{
		List<String> returnValue = new ArrayList<String>();

		Set<Descriptor> keySet = serviceMap.keySet();
		for (Descriptor d : keySet)
			if(d.clazz.equals(clazz))
				returnValue.add(d.name);


		return returnValue.toArray(
			new String[returnValue.size()]);
	} 

	
	
	public static Object getService(String clazz, String name)
	{
		
		Descriptor key = new Descriptor(clazz,name);
		Descriptor value = serviceMap.get(key);
		if(value == null)
		{
			
			return null;
		}
		else
		{
			if(value.code == null)
			{
				loadServices(value.plugin,
					value.plugin.getServicesURI(),
					null);
				value = serviceMap.get(key);
			}
			return value.getInstance();
		}
	} 

	

	
	
	static void registerService(Descriptor d)
	{
		serviceMap.put(d,d);
	} 

	

	
	private static final Map<Descriptor, Descriptor> serviceMap = new HashMap<Descriptor, Descriptor>();
	

	
	static class Descriptor
	{
		final String clazz;
		final String name;
		String code;
		PluginJAR plugin;
		Object instance;
		boolean instanceIsNull;

		
		Descriptor(String clazz, String name)
		{
			this.clazz = clazz;
			this.name  = name;
		}

		
		Descriptor(String clazz, String name, String code,
			PluginJAR plugin)
		{
			this.clazz  = clazz;
			this.name   = name;
			this.code   = code;
			this.plugin = plugin;
		}

		Object getInstance()
		{
			if(instanceIsNull)
				return null;
			else if(instance == null)
			{
				
				instance = BeanShell.eval(null,
					BeanShell.getNameSpace(),
					code);
				if(instance == null)
				{
					
					
					instanceIsNull = true;
				}
			}

			return instance;
		}
		public int hashCode()
		{
			return name.hashCode();
		}

		public boolean equals(Object o)
		{
			if(o instanceof Descriptor)
			{
				Descriptor d = (Descriptor)o;
				return d.clazz.equals(clazz)
					&& d.name.equals(name);
			}
			else
				return false;
		}
	} 
}
