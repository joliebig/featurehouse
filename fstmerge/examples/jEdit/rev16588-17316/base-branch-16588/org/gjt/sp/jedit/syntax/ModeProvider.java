
package org.gjt.sp.jedit.syntax;


import org.gjt.sp.jedit.Mode;
import org.gjt.sp.util.IOUtilities;
import org.gjt.sp.util.Log;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;



public class ModeProvider
{
	public static ModeProvider instance = new ModeProvider();

	private List<Mode> modes = new ArrayList<Mode>(180);

	
	public void removeAll()
	{
		modes = new ArrayList<Mode>(180);
	} 

	
	
	public Mode getMode(String name)
	{
		for(int i = 0; i < modes.size(); i++)
		{
			Mode mode = modes.get(i);
			if(mode.getName().equals(name))
				return mode;
		}
		return null;
	} 

	
	
	public Mode getModeForFile(String filename, String firstLine)
	{
		String nogzName = filename.substring(0,filename.length() -
			(filename.endsWith(".gz") ? 3 : 0));
		Mode[] modes = getModes();

		
		
		for(int i = modes.length - 1; i >= 0; i--)
		{
			if(modes[i].accept(nogzName,firstLine))
			{
				return modes[i];
			}
		}
		return null;
	} 

	
	
	
	public Mode[] getModes()
	{
		Mode[] array = new Mode[modes.size()];
		modes.toArray(array);
		return array;
	} 

	
	
	public void addMode(Mode mode)
	{
		modes.add(mode);
	} 

	
	public void loadMode(Mode mode, XModeHandler xmh)
	{
		String fileName = (String)mode.getProperty("file");

		Log.log(Log.NOTICE,this,"Loading edit mode " + fileName);

		XMLReader parser;
		try
		{
			parser = XMLReaderFactory.createXMLReader();
		} catch (SAXException saxe)
		{
			Log.log(Log.ERROR, this, saxe);
			return;
		}
		mode.setTokenMarker(xmh.getTokenMarker());

		InputStream grammar;

		try
		{
			grammar = new BufferedInputStream(
					new FileInputStream(fileName));
		}
		catch (FileNotFoundException e1)
		{
			InputStream resource = ModeProvider.class.getResourceAsStream(fileName);
			if (resource == null)
				error(fileName, e1);
			grammar = new BufferedInputStream(resource);
		}

		try
		{
			InputSource isrc = new InputSource(grammar);
			isrc.setSystemId("jedit.jar");
			parser.setContentHandler(xmh);
			parser.setDTDHandler(xmh);
			parser.setEntityResolver(xmh);
			parser.setErrorHandler(xmh);
			parser.parse(isrc);

			mode.setProperties(xmh.getModeProperties());
		}
		catch (Throwable e)
		{
			error(fileName, e);
		}
		finally
		{
			IOUtilities.closeQuietly(grammar);
		}
	} 

	
	public void loadMode(Mode mode)
	{
		XModeHandler xmh = new XModeHandler(mode.getName())
		{
			@Override
			public void error(String what, Object subst)
			{
				Log.log(Log.ERROR, this, subst);
			}

			@Override
			public TokenMarker getTokenMarker(String modeName)
			{
				Mode mode = getMode(modeName);
				if(mode == null)
					return null;
				else
					return mode.getTokenMarker();
			}
		};
		loadMode(mode, xmh);
	} 

	
	protected void error(String file, Throwable e)
	{
		Log.log(Log.ERROR, this, e);
	} 
}
