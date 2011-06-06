

package org.gjt.sp.jedit;

import java.io.*;
import java.util.*;

class PropertyManager
{
	
	Properties getProperties()
	{
		Properties total = new Properties();
		total.putAll(system);
		Iterator iter = plugins.iterator();
		while(iter.hasNext())
			total.putAll((Properties)iter.next());
		total.putAll(user);
		return total;
	} 

	
	void loadSystemProps(InputStream in)
		throws IOException
	{
		loadProps(system,in);
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
		out.close();
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
				user.put(name,"");
		}
		else
		{
			if(value.equals(prop))
				user.remove(name);
			else
				user.put(name,value);
		}
	} 

	
	public void setTemporaryProperty(String name, String value)
	{
		user.remove(name);
		system.put(name,value);
	} 

	
	void unsetProperty(String name)
	{
		if(getDefaultProperty(name) != null)
			user.put(name,"");
		else
			user.remove(name);
	} 

	
	public void resetProperty(String name)
	{
		user.remove(name);
	} 

	
	private Properties system = new Properties();
	private List plugins = new LinkedList();
	private Properties user = new Properties();

	
	private String getDefaultProperty(String name)
	{
		Iterator iter = plugins.iterator();
		while(iter.hasNext())
		{
			String value = ((Properties)iter.next()).getProperty(
				name);
			if(value != null)
				return value;
		}
		return system.getProperty(name);
	} 

	
	private void loadProps(Properties into, InputStream in)
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
