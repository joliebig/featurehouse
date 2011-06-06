

package org.gjt.sp.jedit;


import com.microstar.xml.*;
import java.io.*;
import java.net.URL;
import java.util.*;
import org.gjt.sp.util.Log;



class ServiceListHandler extends HandlerBase
{
	
	ServiceListHandler(PluginJAR plugin, URL uri)
	{
		this.plugin = plugin;
		this.uri = uri;
		stateStack = new Stack();
		cachedServices = new LinkedList();
	} 

	
	public Object resolveEntity(String publicId, String systemId)
	{
		if("services.dtd".equals(systemId))
		{
			
			
			
			return new StringReader("<!-- -->");

			
		}

		return null;
	} 

	
	public void attribute(String aname, String value, boolean isSpecified)
	{
		if(aname.equals("NAME"))
			serviceName = value;
		else if(aname.equals("CLASS"))
			serviceClass = value;
	} 

	
	public void doctypeDecl(String name, String publicId,
		String systemId) throws Exception
	{
		if("SERVICES".equals(name))
			return;

		Log.log(Log.ERROR,this,uri + ": DOCTYPE must be SERVICES");
	} 

	
	public void charData(char[] c, int off, int len)
	{
		String tag = peekElement();
		String text = new String(c, off, len);

		if (tag == "SERVICE")
		{
			code = text;
		}
	} 

	
	public void startElement(String tag)
	{
		tag = pushElement(tag);
	} 

	
	public void endElement(String name)
	{
		if(name == null)
			return;

		String tag = peekElement();

		if(name.equals(tag))
		{
			if(tag == "SERVICE")
			{
				ServiceManager.Descriptor d =
					new ServiceManager.Descriptor(
					serviceClass,serviceName,code,plugin);
				ServiceManager.registerService(d);
				cachedServices.add(d);
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
	private String code;

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
