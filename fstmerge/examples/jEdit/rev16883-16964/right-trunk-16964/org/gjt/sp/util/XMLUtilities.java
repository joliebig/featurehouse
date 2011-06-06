

package org.gjt.sp.util;


import java.io.InputStream;
import java.io.BufferedInputStream;
import java.io.IOException;

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
		StringBuilder buf = new StringBuilder(str.length());
		for(int i = 0; i < str.length(); i++)
		{
			char ch = str.charAt(i);

			
			
			if (((0x00 <= ch && ch <= 0x1F) || (0x7F <= ch && ch <= 0x9F))
				&& ch != '\r' && ch != '\n' && ch != '\t')
			{
				if (xml11 && ch != 0x00)
				{
					buf.append("&#").append((int)ch).append(';');
				}
				else
				{
					
					
					
					buf.append("<?illegal-xml-character ")
						.append((int)ch)
						.append("?>");
				}
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
		try
		{
			XMLReader parser = XMLReaderFactory.createXMLReader();
			InputSource isrc = new InputSource(
				new BufferedInputStream(in));
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
				return new InputSource(new BufferedInputStream(
					where.getResourceAsStream(test)));
			}
			catch (Exception e)
			{
				Log.log(Log.ERROR,XMLUtilities.class,
					"Error while opening " + test + ':');
				Log.log(Log.ERROR,XMLUtilities.class,e);
			}
		}

		return null;
	} 

	private XMLUtilities() { }
}

