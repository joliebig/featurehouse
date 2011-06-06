

package org.gjt.sp.jedit.pluginmgr;

import com.microstar.xml.*;
import java.io.*;
import java.util.*;
import org.gjt.sp.util.Log;

class MirrorListHandler extends HandlerBase
{
	
	MirrorListHandler(MirrorList mirrors, String path)
	{
		this.mirrors = mirrors;
		this.path = path;
		stateStack = new Stack();
	} 

	
	public Object resolveEntity(String publicId, String systemId)
	{
		if("mirrors.dtd".equals(systemId))
		{
			
			
			
			return new StringReader("<!-- -->");
		}

		return null;
	} 

	
	public void attribute(String aname, String value, boolean isSpecified)
	{
		aname = (aname == null) ? null : aname.intern();
		value = (value == null) ? null : value.intern();
		if(aname == "ID")
			id = value;
	} 

	
	public void doctypeDecl(String name, String publicId,
		String systemId) throws Exception
	{
		if("MIRRORS".equals(name))
			return;

		Log.log(Log.ERROR,this,path + ": DOCTYPE must be MIRRORS");
	} 

	
	public void charData(char[] c, int off, int len)
	{
		String tag = peekElement();
		String text = new String(c, off, len);
		
		if(tag == "DESCRIPTION")
			description = text;
		else if(tag == "LOCATION")
			location = text;
		else if(tag == "COUNTRY")
			country = text;
		else if(tag == "CONTINENT")
			continent = text;
	} 

	
	public void startElement(String tag)
	{
		tag = pushElement(tag);

		if(tag == "MIRROR")
			mirror = new MirrorList.Mirror();
	} 

	
	public void endElement(String tag)
	{
		if(tag == null)
			return;
		else
			tag = tag.intern();

		popElement();

		if(tag == "MIRROR")
		{
			mirror.id = id;
			mirror.description = description;
			mirror.location = location;
			mirror.country = country;
			mirror.continent = continent;
			mirrors.add(mirror);
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

	
	public void endDocument()
	{
		mirrors.finished();
	} 

	
	
	
	private String id;
	private String description;
	private String location;
	private String country;
	private String continent;
	
	private MirrorList mirrors;
	private MirrorList.Mirror mirror;
	
	private Stack stateStack;
	private String path;
	
	
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
