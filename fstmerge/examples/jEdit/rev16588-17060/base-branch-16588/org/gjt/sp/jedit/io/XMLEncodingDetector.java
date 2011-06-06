

package org.gjt.sp.jedit.io;


import java.io.InputStream;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.IllegalCharsetNameException;

import org.gjt.sp.util.Log;



public class XMLEncodingDetector implements EncodingDetector
{
	
	public String detectEncoding(InputStream sample) throws IOException
	{
		
		
		final int XML_PI_LENGTH = 50;
		
		byte[] _xmlPI = new byte[XML_PI_LENGTH];
		int offset = 0;
		int count;
		while((count = sample.read(_xmlPI,offset,
			XML_PI_LENGTH - offset)) != -1)
		{
			offset += count;
			if(offset == XML_PI_LENGTH)
				break;
		}
		return getXMLEncoding(new String(_xmlPI,0,offset,"ASCII"));
	} 

	
	
	private static String getXMLEncoding(String xmlPI)
	{
		if(!xmlPI.startsWith("<?xml"))
			return null;

		int index = xmlPI.indexOf("encoding=");
		if(index == -1 || index + 9 == xmlPI.length())
			return null;

		char ch = xmlPI.charAt(index + 9);
		int endIndex = xmlPI.indexOf(ch,index + 10);
		if(endIndex == -1)
			return null;

		String encoding = xmlPI.substring(index + 10,endIndex);

		try
		{
			if(Charset.isSupported(encoding))
			{
				return encoding;
			}
			else
			{
				Log.log(Log.WARNING, XMLEncodingDetector.class,
					"XML PI specifies unsupported encoding: " + encoding);
			}
		}
		catch(IllegalCharsetNameException e)
		{
			Log.log(Log.WARNING, XMLEncodingDetector.class,
				"XML PI specifies illegal encoding: " + encoding,
				e);
		}
		return null;
	}
	
}
