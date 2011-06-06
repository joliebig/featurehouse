

package org.gjt.sp.jedit;


import java.net.URL;
import java.util.*;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.helpers.DefaultHandler;

import org.gjt.sp.util.XMLUtilities;
import org.gjt.sp.util.Log;



class ServiceListHandler extends DefaultHandler
{
	
	ServiceListHandler(PluginJAR plugin, URL uri)
	{
		this.plugin = plugin;
		this.uri = uri;
		code = new StringBuilder();
		stateStack = new Stack<String>();
		cachedServices = new LinkedList<ServiceManager.Descriptor>();
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
		pushElement(tag);
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
			Log.log(Log.ERROR, e, e);
		}
	} 

	
	public ServiceManager.Descriptor[] getCachedServices()
	{
		return cachedServices.toArray(
			new ServiceManager.Descriptor[cachedServices.size()]);
	} 

	

	
	private PluginJAR plugin;
	private URL uri;

	private String serviceName;
	private String serviceClass;
	private StringBuilder code;

	private Stack<String> stateStack;

	private List<ServiceManager.Descriptor> cachedServices;
	

	
	private String pushElement(String name)
	{
		name = (name == null) ? null : name.intern();

		stateStack.push(name);

		return name;
	} 

	
	private String peekElement()
	{
		return stateStack.peek();
	} 

	
	private String popElement()
	{
		return stateStack.pop();
	} 

	
}
