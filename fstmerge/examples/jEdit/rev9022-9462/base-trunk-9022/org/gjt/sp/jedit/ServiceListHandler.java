

package org.gjt.sp.jedit;


import java.io.*;
import java.net.URL;
import java.util.*;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.helpers.DefaultHandler;

import org.gjt.sp.util.Log;
import org.gjt.sp.util.XMLUtilities;



class ServiceListHandler extends DefaultHandler
{
	
	ServiceListHandler(PluginJAR plugin, URL uri)
	{
		this.plugin = plugin;
		this.uri = uri;
		code = new StringBuffer();
		stateStack = new Stack();
		cachedServices = new LinkedList();
	} 

	
	public InputSource resolveEntity(String publicId, String systemId)
	{
		return XMLUtilities.findEntity(systemId, "services.dtd", getClass());
	} 

	
	public void characters(char[] c, int off, int len)
	{
		String tag = peekElement();
		if (tag == "SERVICE")
			code.append(c, off, len);
	} 

	
	public void startElement(String uri, String localName,
				 String tag, Attributes attrs)
	{
		tag = pushElement(tag);
		serviceName = attrs.getValue("NAME");
		serviceClass = attrs.getValue("CLASS");
	} 

	
	public void endElement(String uri, String localName, String name)
	{
		String tag = peekElement();

		if(name.equals(tag))
		{
			if (tag.equals("SERVICE"))
			{
				ServiceManager.Descriptor d =
					new ServiceManager.Descriptor(
					serviceClass,serviceName,code.toString(),plugin);
				ServiceManager.registerService(d);
				cachedServices.add(d);
				code.setLength(0);
			}

			popElement();
		}
		else
		{
			
			throw new InternalError();
		}
	} 

	
	public void startDocument()
	{
		try
		{
			pushElement(null);
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	} 

	
	public ServiceManager.Descriptor[] getCachedServices()
	{
		return (ServiceManager.Descriptor[])cachedServices.toArray(
			new ServiceManager.Descriptor[cachedServices.size()]);
	} 

	

	
	private PluginJAR plugin;
	private URL uri;

	private String serviceName;
	private String serviceClass;
	private StringBuffer code;

	private Stack stateStack;

	private List cachedServices;
	

	
	private String pushElement(String name)
	{
		name = (name == null) ? null : name.intern();

		stateStack.push(name);

		return name;
	} 

	
	private String peekElement()
	{
		return (String) stateStack.peek();
	} 

	
	private String popElement()
	{
		return (String) stateStack.pop();
	} 

	
}
