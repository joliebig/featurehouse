

package org.gjt.sp.jedit.pluginmgr;

import java.util.*;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.helpers.DefaultHandler;

import org.gjt.sp.util.XMLUtilities;
import org.gjt.sp.util.Log;
import org.gjt.sp.jedit.options.PluginOptions;


class MirrorListHandler extends DefaultHandler
{
	
	MirrorListHandler(MirrorList mirrors, String path)
	{
		this.mirrors = mirrors;
		this.path = path;
	} 

	
	public InputSource resolveEntity(String publicId, String systemId)
	{
		return XMLUtilities.findEntity(systemId, "mirrors.dtd",
					PluginOptions.class);
	} 

	
	public void characters(char[] c, int off, int len)
	{
		String tag = peekElement();

		if(tag == "DESCRIPTION")
			description.append(c, off, len);
		else if(tag == "LOCATION")
			location.append(c, off, len);
		else if(tag == "COUNTRY")
			country.append(c, off, len);
		else if(tag == "CONTINENT")
			continent.append(c, off, len);
	} 

	
	public void startElement(String uri, String localName,
				 String tag, Attributes attrs)
	{
		tag = pushElement(tag);

		if (tag.equals("MIRROR"))
		{
			mirror = new MirrorList.Mirror();
			id = attrs.getValue("ID");
		}
	} 

	
	public void endElement(String uri, String localName, String tag)
	{
		popElement();

		if(tag.equals("MIRROR"))
		{
			mirror.id = id;
			mirror.description = description.toString();
			mirror.location = location.toString();
			mirror.country = country.toString();
			mirror.continent = continent.toString();
			mirrors.add(mirror);
			description.setLength(0);
			location.setLength(0);
			country.setLength(0);
			continent.setLength(0);
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
			Log.log(Log.ERROR, this, e);
		}
	} 

	
	public void endDocument()
	{
		mirrors.finished();
	} 

	

	
	private String id;
	private final StringBuilder description = new StringBuilder();
	private final StringBuilder location = new StringBuilder();
	private final StringBuilder country = new StringBuilder();
	private final StringBuilder continent = new StringBuilder();

	private final MirrorList mirrors;
	private MirrorList.Mirror mirror;

	private final Stack<String> stateStack = new Stack<String>();
	private final String path;
	

	private String pushElement(String name)
	{
		name = name == null ? null : name.intern();
		stateStack.push(name);
		return name;
	}

	private String peekElement()
	{
		return stateStack.peek();
	}

	private void popElement()
	{
		stateStack.pop();
	}

	
}
