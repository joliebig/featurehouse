

package org.gjt.sp.util;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.Reader;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;


public class XMLUtilities
{

	
	
	public static String charsToEntities(String str, boolean xml11)
	{
		StringBuffer buf = new StringBuffer(str.length());
		for(int i = 0; i < str.length(); i++)
		{
			char ch = str.charAt(i);

			
			if (xml11 && ch < 32 && ch != '\r' && ch != '\n' && ch != '\t')
			{
				buf.append("&#").append((int)ch).append(";");
				continue;
			}

			switch(ch)
			{
			case '<':
				buf.append("&lt;");
				break;
			case '>':
				buf.append("&gt;");
				break;
			case '&':
				buf.append("&amp;");
				break;
			default:
				buf.append(ch);
				break;
			}
		}
		return buf.toString();
	} 

	
	
	public static boolean parseXML(InputStream in, DefaultHandler handler)
		throws IOException
	{
		Reader r = null;
		try
		{
			XMLReader parser = XMLReaderFactory.createXMLReader();
			r = new BufferedReader(new InputStreamReader(in));
			InputSource isrc = new InputSource(r);
			isrc.setSystemId("jedit.jar");
			parser.setContentHandler(handler);
			parser.setDTDHandler(handler);
			parser.setEntityResolver(handler);
			parser.setErrorHandler(handler);
			parser.parse(isrc);
		}
		catch(SAXParseException se)
		{
			int line = se.getLineNumber();
			String message = se.getMessage();
			Log.log(Log.ERROR,XMLUtilities.class,
				"while parsing from " + in + ": SAXParseException: line " + line + ": " , se);
			return true;
		}
		catch(SAXException e)
		{
			Log.log(Log.ERROR,XMLUtilities.class,e);
			return true;
		}
		finally
		{
			try
			{
				if(in != null)
					in.close();
			}
			catch(IOException io)
			{
				Log.log(Log.ERROR,XMLUtilities.class,io);
			}
		}
		return false;
	} 

	
	
	public static InputSource findEntity(String systemId, String test, Class where)
	{
		if (systemId != null && systemId.endsWith(test))
		{
			try
			{
				return new InputSource(new BufferedReader(
					new InputStreamReader(
						where.getResourceAsStream(test))));
			}
			catch (Exception e)
			{
				Log.log(Log.ERROR,XMLUtilities.class,
					"Error while opening " + test + ":");
				Log.log(Log.ERROR,XMLUtilities.class,e);
			}
		}

		return null;
	} 

	private XMLUtilities() { }
}

