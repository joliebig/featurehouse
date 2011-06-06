

package org.gjt.sp.jedit.proto.jeditresource;


import java.io.*;
import java.net.*;
import org.gjt.sp.jedit.*;


public class PluginResURLConnection extends URLConnection
{
	public PluginResURLConnection(URL url)
		throws IOException
	{
		super(url);

		String file = url.getFile();

		int index = file.indexOf('!',0);
		if(index == -1)
		{
			plugin = null;
			resource = file;
		}
		else
		{
			int start;
			if(file.charAt(0) == '/')
				start = 1;
			else
				start = 0;

			plugin = file.substring(start,index);
			resource = file.substring(index + 1);
		}

		if(plugin != null && resource.startsWith("/"))
			resource = resource.substring(1);
	}

	public void connect() throws IOException
	{
		if(!connected)
		{
			if(plugin == null)
			{
				in = jEdit.class.getResourceAsStream(resource);
			}
			else
			{
				PluginJAR[] plugins = jEdit.getPluginJARs();
				for(int i = 0; i < plugins.length; i++)
				{
					PluginJAR jar = plugins[i];
					if(MiscUtilities.getFileName(jar.getPath())
						.equalsIgnoreCase(plugin))
					{
						in = jar.getClassLoader()
							.getResourceAsStream(
							resource);
						break;
					}
				}
			}

			if(in == null)
			{
				throw new IOException("Resource not found: "
					+ resource);
			}

			connected = true;
		}
	}

	public InputStream getInputStream()
		throws IOException
	{
		connect();
		return in;
	}

	public String getHeaderField(String name)
	{
		if(name.equals("content-type"))
		{
			String lcResource = resource.toLowerCase();
			if(lcResource.endsWith(".html"))
				return "text/html";
			else if(lcResource.endsWith(".txt"))
				return "text/plain";
			else if(lcResource.endsWith(".rtf"))
				return "text/rtf";
			else if(lcResource.endsWith(".gif"))
				return "image/gif";
			else if(lcResource.endsWith(".jpg")
				|| lcResource.endsWith(".jpeg"))
				return "image/jpeg";
			else
				return null;
		}
		else
			return null;
	}

	
	private InputStream in;
	private String plugin;
	private String resource;
}
