

package org.gjt.sp.jedit;

import com.microstar.xml.*;
import java.io.*;
import java.net.URL;
import java.util.*;
import org.gjt.sp.util.Log;


public class ServiceManager
{
	
	
	public static void loadServices(PluginJAR plugin, URL uri,
		PluginJAR.PluginCacheEntry cache)
	{
		Reader in = null;

		try
		{
			Log.log(Log.DEBUG,jEdit.class,"Loading services from " + uri);

			ServiceListHandler dh = new ServiceListHandler(plugin,uri);
			XmlParser parser = new XmlParser();
			parser.setHandler(dh);
			in = new BufferedReader(
				new InputStreamReader(
				uri.openStream()));
			parser.parse(null, null, in);
			if(cache != null)
				cache.cachedServices = dh.getCachedServices();
		}
		catch(XmlException xe)
		{
			int line = xe.getLine();
			String message = xe.getMessage();
			Log.log(Log.ERROR,ServiceManager.class,uri + ":" + line
				+ ": " + message);
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,ServiceManager.class,e);
		}
		finally
		{
			try
			{
				if(in != null)
					in.close();
			}
			catch(IOException io)
			{
				Log.log(Log.ERROR,ServiceManager.class,io);
			}
		}
	} 

	
	
	public static void unloadServices(PluginJAR plugin)
	{
		Iterator descriptors = serviceMap.keySet().iterator();
		while(descriptors.hasNext())
		{
			Descriptor d = (Descriptor)descriptors.next();
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
		HashSet returnValue = new HashSet();

		Iterator descriptors = serviceMap.keySet().iterator();
		while(descriptors.hasNext())
		{
			Descriptor d = (Descriptor)descriptors.next();
			returnValue.add(d.clazz);
		}

		return (String[])returnValue.toArray(
			new String[returnValue.size()]);
	} 

	
	
	public static String[] getServiceNames(String clazz)
	{
		ArrayList returnValue = new ArrayList();

		Iterator descriptors = serviceMap.keySet().iterator();
		while(descriptors.hasNext())
		{
			Descriptor d = (Descriptor)descriptors.next();
			if(d.clazz.equals(clazz))
				returnValue.add(d.name);
		}

		return (String[])returnValue.toArray(
			new String[returnValue.size()]);
	} 

	
	
	public static Object getService(String clazz, String name)
	{
		
		Descriptor key = new Descriptor(clazz,name);
		Descriptor value = (Descriptor)serviceMap.get(key);
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
				value = (Descriptor)serviceMap.get(key);
			}
			return value.getInstance();
		}
	} 

	

	
	
	static void registerService(Descriptor d)
	{
		serviceMap.put(d,d);
	} 

	

	
	private static Map serviceMap = new HashMap();
	

	
	static class Descriptor
	{
		String clazz;
		String name;
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
