

package org.gjt.sp.jedit.pluginmgr;

import java.io.*;
import java.net.*;
import java.util.*;

import org.xml.sax.XMLReader;
import org.xml.sax.InputSource;
import org.xml.sax.helpers.XMLReaderFactory;

import org.gjt.sp.jedit.*;
import org.gjt.sp.util.IOUtilities;
import org.gjt.sp.util.ProgressObserver;
import org.gjt.sp.util.Log;


public class MirrorList
{
	public List<Mirror> mirrors;
	
	public String xml;

	
	public MirrorList(boolean download, ProgressObserver observer) throws Exception
	{
		mirrors = new ArrayList<Mirror>();

		Mirror none = new Mirror();
		none.id = Mirror.NONE;
		none.description = none.location = none.country = none.continent = "";
		mirrors.add(none);

		String path = jEdit.getProperty("plugin-manager.mirror-url");
		MirrorListHandler handler = new MirrorListHandler(this,path);
		if (download)
		{
			Log.log(Log.NOTICE, this, "Loading mirror list from internet");
			downloadXml(path);
		}
		else
		{
			Log.log(Log.NOTICE, this, "Loading mirror list from cache");
			readXml();
		}
		observer.setValue(1);
		Reader in = new BufferedReader(new StringReader(xml));

		InputSource isrc = new InputSource(in);
		isrc.setSystemId("jedit.jar");
		XMLReader parser = XMLReaderFactory.createXMLReader();
		parser.setContentHandler(handler);
		parser.setDTDHandler(handler);
		parser.setEntityResolver(handler);
		parser.setErrorHandler(handler);
		parser.parse(isrc);
		observer.setValue(2);
	} 

	

	
	
	private void readXml() throws IOException
	{
		String settingsDirectory = jEdit.getSettingsDirectory();
		if(settingsDirectory == null)
			return;

		File mirrorList = new File(MiscUtilities.constructPath(
			settingsDirectory,"mirrorList.xml"));
		if(!mirrorList.exists())
			return;
		InputStream inputStream = null;
		try
		{
			inputStream = new BufferedInputStream(new FileInputStream(mirrorList));

			ByteArrayOutputStream out = new ByteArrayOutputStream();
			IOUtilities.copyStream(null,inputStream,out, false);
			xml = out.toString();
		}
		finally
		{
			IOUtilities.closeQuietly(inputStream);
		}
	} 

	
	
	private void downloadXml(String path) throws IOException
	{
		InputStream inputStream = null;
		try
		{
			inputStream = new URL(path).openStream();

			ByteArrayOutputStream out = new ByteArrayOutputStream();
			IOUtilities.copyStream(null,inputStream,out, false);
			xml = out.toString();
		}
		finally
		{
			IOUtilities.closeQuietly(inputStream);
		}
	} 

	
	void add(Mirror mirror)
	{
		mirrors.add(mirror);
	} 

	
	void finished()
	{
		Collections.sort(mirrors,new MirrorCompare());
	} 

	

	

	
	public static class Mirror
	{
		public static final String NONE = "NONE";

		public String id;
		public String description;
		public String location;
		public String country;
		public String continent;
	} 

	
	static private class MirrorCompare implements Comparator<Mirror>
	{
		public int compare(Mirror m1,Mirror m2)
		{
			int result;
			if ((result = m1.continent.compareToIgnoreCase(m2.continent)) == 0)
				if ((result = m1.country.compareToIgnoreCase(m2.country)) == 0)
					if ((result = m1.location.compareToIgnoreCase(m2.location)) == 0)
						return m1.description.compareToIgnoreCase(m2.description);
			return result;
		}

		public boolean equals(Object obj)
		{
			return obj instanceof MirrorCompare;
		}
	} 

	
}
