

package org.gjt.sp.jedit;

import java.io.*;
import java.util.*;

class PropertyManager
{
	
	Properties getProperties()
	{
		Properties total = new Properties();
		total.putAll(system);
		for (Properties plugin : plugins)
			total.putAll(plugin);
		total.putAll(site);
		total.putAll(user);
		return total;
	} 

	
	void loadSystemProps(InputStream in)
		throws IOException
	{
		loadProps(system,in);
	} 

	
	void loadSiteProps(InputStream in)
		throws IOException
	{
		loadProps(site,in);
	} 

	
	void loadUserProps(InputStream in)
		throws IOException
	{
		loadProps(user,in);
	} 

	
	void saveUserProps(OutputStream out)
		throws IOException
	{
		user.store(out,"jEdit properties");
	} 

	
	Properties loadPluginProps(InputStream in)
		throws IOException
	{
		Properties plugin = new Properties();
		loadProps(plugin,in);
		plugins.add(plugin);
		return plugin;
	} 

	
	void addPluginProps(Properties props)
	{
		plugins.add(props);
	} 

	
	void removePluginProps(Properties props)
	{
		plugins.remove(props);
	} 

	
	String getProperty(String name)
	{
		String value = user.getProperty(name);
		if(value != null)
			return value;
		else
			return getDefaultProperty(name);
	} 

	
	void setProperty(String name, String value)
	{
		String prop = getDefaultProperty(name);

		
		if(value == null)
		{
			if(prop == null || prop.length() == 0)
				user.remove(name);
			else
				user.setProperty(name,"");
		}
		else
		{
			if(value.equals(prop))
				user.remove(name);
			else
				user.setProperty(name,value);
		}
	} 

	
	public void setTemporaryProperty(String name, String value)
	{
		user.remove(name);
		system.setProperty(name,value);
	} 

	
	void unsetProperty(String name)
	{
		if(getDefaultProperty(name) != null)
			user.setProperty(name,"");
		else
			user.remove(name);
	} 

	
	public void resetProperty(String name)
	{
		user.remove(name);
	} 

	
	private Properties system = new Properties();
	private List<Properties> plugins = new LinkedList<Properties>();
	private Properties site = new Properties();
	private Properties user = new Properties();

	
	private String getDefaultProperty(String name)
	{
		String value = site.getProperty(name);
		if(value != null)
			return value;

		for (Properties plugin : plugins)
		{
			value = plugin.getProperty(name);
			if (value != null)
				return value;
		}

		return system.getProperty(name);
	} 

	
	private static void loadProps(Properties into, InputStream in)
		throws IOException
	{
		try
		{
			into.load(in);
		}
		finally
		{
			in.close();
		}
	} 

	
}
