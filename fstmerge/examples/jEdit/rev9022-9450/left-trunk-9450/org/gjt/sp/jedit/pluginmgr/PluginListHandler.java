

package org.gjt.sp.jedit.pluginmgr;

import java.util.Stack;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.helpers.DefaultHandler;

import org.gjt.sp.util.Log;
import org.gjt.sp.util.XMLUtilities;



class PluginListHandler extends DefaultHandler
{
	PluginListHandler(PluginList pluginList, String path)
	{
		this.pluginList = pluginList;
		this.path = path;

		author = new StringBuffer();
		description = new StringBuffer();
		pluginSetEntry = new StringBuffer();
		download = new StringBuffer();
		downloadSource = new StringBuffer();
	}

	public InputSource resolveEntity(String publicId, String systemId)
	{
		return XMLUtilities.findEntity(systemId, "plugins.dtd", getClass());
	}

	public void attribute(String aname, String value, boolean isSpecified)
	{
		if(aname == "NAME")
			name = value;
		else if(aname == "JAR")
			jar = value;
		else if(aname == "VERSION")
			version = value;
		else if(aname == "DATE")
			date = value;
		else if(aname == "OBSOLETE")
			obsolete = ("TRUE".equals(value));
		else if(aname == "WHAT")
			depWhat = value;
		else if(aname == "FROM")
			depFrom = value;
		else if(aname == "TO")
			depTo = value;
		else if(aname == "PLUGIN")
			depPlugin = value;
		else if(aname == "SIZE")
		{
			size = Integer.parseInt(value);
			if(size == 0)
				Log.log(Log.WARNING,this,"SIZE = 0");
		}
	}

	public void characters(char[] c, int off, int len)
	{
		String tag = peekElement();

		if(tag.equals("DESCRIPTION"))
		{
			description.append(c, off, len);
		}
		else if(tag.equals("PLUGIN_SET_ENTRY"))
			pluginSetEntry.append(c, off, len);
		else if(tag.equals("AUTHOR"))
		{
			if(author.length() != 0)
				author.append(", ");
			author.append(c, off, len);
		}
		else if(tag.equals("DOWNLOAD"))
			download.append(c, off, len);
		else if(tag.equals("DOWNLOAD_SOURCE"))
			downloadSource.append(c, off, len);
	}

	public void startElement(String uri, String localName,
				 String tag, Attributes attrs)
	{
		for (int i = 0; i < attrs.getLength(); i++)
		{
			String aName = attrs.getQName(i);
			String aValue = attrs.getValue(i);
			attribute(aName, aValue, true);
		}


		tag = pushElement(tag);

		if(tag.equals("PLUGIN_SET"))
		{
			description.setLength(0);
			pluginSet = new PluginList.PluginSet();
			pluginSet.name = name;
		}
		else if(tag.equals("PLUGIN"))
		{
			description.setLength(0);
			author.setLength(0);
			branch = null;
			plugin = new PluginList.Plugin();
		}
		else if(tag.equals("BRANCH"))
		{
			download.setLength(0);
			branch = new PluginList.Branch();
		}
		else if(tag.equals("DOWNLOAD"))
			downloadSize = size;
		else if(tag.equals("DOWNLOAD_SOURCE"))
			downloadSourceSize = size;
	}

	public void endElement(String uri, String localName, String tag)
	{
		popElement();

		if(tag.equals("PLUGIN_SET"))
		{
			pluginList.addPluginSet(pluginSet);
			pluginSet = null;
			pluginSetEntry.setLength(0);
		}
		else if(tag.equals("PLUGIN_SET_ENTRY"))
		{
			pluginSet.plugins.add(pluginSetEntry.toString());
			pluginSetEntry.setLength(0);
		}
		else if(tag.equals("PLUGIN"))
		{
			plugin.jar = jar;
			plugin.name = name;
			plugin.author = author.toString();
			plugin.description = description.toString();
			pluginList.addPlugin(plugin);
			jar = null;
			name = null;
			author.setLength(0);
			description.setLength(0);
		}
		else if(tag.equals("BRANCH"))
		{
			branch.version = version;
			branch.date = date;
			branch.download = download.toString();
			branch.downloadSize = downloadSize;
			branch.downloadSource = downloadSource.toString();
			branch.downloadSourceSize = downloadSourceSize;
			branch.obsolete = obsolete;
			plugin.branches.add(branch);
			version = null;
			download.setLength(0);
			downloadSource.setLength(0);
			obsolete = false;
		}
		else if(tag.equals("DEPEND"))
		{
			PluginList.Dependency dep = new PluginList.Dependency(
				depWhat,depFrom,depTo,depPlugin);
			branch.deps.add(dep);
			depWhat = null;
			depFrom = null;
			depTo = null;
			depPlugin = null;
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
		pluginList.finished();
	}
	

	
	private String path;

	private PluginList pluginList;

	private PluginList.PluginSet pluginSet;
	private StringBuffer pluginSetEntry;

	private PluginList.Plugin plugin;
	private String jar;
	private StringBuffer author;

	private PluginList.Branch branch;
	private boolean obsolete;
	private String version;
	private String date;
	private StringBuffer download;
	private int downloadSize;
	private StringBuffer downloadSource;
	private int downloadSourceSize;
	private int size;
	private String depWhat;
	private String depFrom;
	private String depTo;
	private String depPlugin;

	private String name;
	private StringBuffer description;

	private final Stack<String> stateStack = new Stack<String>();

	private String pushElement(String name)
	{
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
