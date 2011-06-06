
package org.gjt.sp.jedit;

import java.util.List;
import java.util.LinkedList;
import java.io.IOException;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.helpers.DefaultHandler;

import org.gjt.sp.jedit.buffer.KillRing;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.XMLUtilities;
import org.gjt.sp.util.IOUtilities;


class JEditKillRing extends KillRing
{
	
	JEditKillRing()
	{
		String settingsDirectory = jEdit.getSettingsDirectory();
		if(settingsDirectory != null)
		{
			killringXML = new SettingsXML(settingsDirectory, "killring");
		}
	} 

	
	@Override
	public void load()
	{
		if(killringXML == null)
			return;

		if(!killringXML.fileExists())
			return;

		Log.log(Log.MESSAGE,KillRing.class,"Loading " + killringXML);

		KillRingHandler handler = new KillRingHandler();
		try
		{
			killringXML.load(handler);
		}
		catch (IOException ioe)
		{
			Log.log(Log.ERROR, this, ioe);
		}
		reset(handler.list);
	} 

	
	@Override
	public void save()
	{
		if(killringXML == null)
			return;

		if(killringXML.hasChangedOnDisk())
		{
			Log.log(Log.WARNING,KillRing.class,killringXML
				+ " changed on disk; will not save killring"
				+ " files");
			return;
		}

		Log.log(Log.MESSAGE,KillRing.class,"Saving " + killringXML);

		String lineSep = System.getProperty("line.separator");

		SettingsXML.Saver out = null;

		try
		{
			out = killringXML.openSaver();
			out.writeXMLDeclaration("1.1");

			out.write("<!DOCTYPE KILLRING SYSTEM \"killring.dtd\">");
			out.write(lineSep);
			out.write("<KILLRING>");
			out.write(lineSep);

			int size = getSize();
			for(int i = size - 1; i >=0; i--)
			{
				out.write("<ENTRY>");
				out.write(XMLUtilities.charsToEntities(
					getElementAt(i).toString(),true));
				out.write("</ENTRY>");
				out.write(lineSep);
			}

			out.write("</KILLRING>");
			out.write(lineSep);

			out.finish();
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,KillRing.class,e);
		}
		finally
		{
			IOUtilities.closeQuietly(out);
		}
	} 

	
	private SettingsXML killringXML;

	
	private static class KillRingHandler extends DefaultHandler
	{
		public List<String> list = new LinkedList<String>();

		@Override
		public InputSource resolveEntity(String publicId, String systemId)
		{
			return XMLUtilities.findEntity(systemId, "killring.dtd", getClass());
		}

		@Override
		public void startElement(String uri, String localName,
					 String qName, Attributes attrs)
		{
			inEntry = qName.equals("ENTRY");
		}

		@Override
		public void endElement(String uri, String localName, String name)
		{
			if(name.equals("ENTRY"))
			{
				list.add(charData.toString());
				inEntry = false;
				charData.setLength(0);
			}
		}

		@Override
		public void characters(char[] ch, int start, int length)
		{
			if (inEntry)
				charData.append(ch, start, length);
		}

		@Override
		public void processingInstruction(String target, String data)
		{
			if ("illegal-xml-character".equals(target))
			{
				char ch;
				try
				{
					ch = (char)Integer.parseInt(data.trim());
				}
				catch (Exception e)
				{
					Log.log(Log.ERROR, this,
						"Failed to get character from PI"
							+ "\"" + target + "\""
							+ " with \"" + data + "\""
							+ ": " + e);
					return;
				}
				characters(new char[] {ch}, 0, 1);
			}
		}

		private final StringBuilder charData = new StringBuilder();
		private boolean inEntry;
	} 
	
}
