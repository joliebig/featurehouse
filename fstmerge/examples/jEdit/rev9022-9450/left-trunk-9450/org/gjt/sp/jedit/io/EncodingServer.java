

package org.gjt.sp.jedit.io;


import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.IllegalCharsetNameException;
import java.nio.charset.UnsupportedCharsetException;
import java.util.Set;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Arrays;

import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.ServiceManager;



public class EncodingServer
{
	
	
	public static Encoding getEncoding(String name)
	{
		try
		{
			return new CharsetEncoding(name);
		}
		catch (java.nio.charset.IllegalCharsetNameException e)
		{
			
		}
		catch (java.nio.charset.UnsupportedCharsetException e)
		{
			
		}

		Object namedService = ServiceManager.getService(serviceClass, name);
		if (namedService != null && namedService instanceof Encoding)
		{
			return (Encoding)namedService;
		}

		throw new IllegalArgumentException("No such encoding: \"" + name + "\"");
	} 

	
	
	public static Set<String> getAvailableNames()
	{
		Set<String> set = new HashSet<String>();
		set.addAll(Charset.availableCharsets().keySet());
		set.addAll(Arrays.asList(ServiceManager.getServiceNames(serviceClass)));
		return set;
	} 

	
	
	public static Set<String> getSelectedNames()
	{
		Set<String> set = getAvailableNames();
		Iterator<String> i = set.iterator();
		while (i.hasNext())
		{
			String name = i.next();
			if (jEdit.getBooleanProperty("encoding.opt-out." + name, false))
			{
				i.remove();
			}
		}
		return set;
	} 

	
	
	public static Reader getTextReader(InputStream in, String encoding)
		throws IOException
	{
		return getEncoding(encoding).getTextReader(in);
	} 

	
	
	public static Writer getTextWriter(OutputStream out, String encoding)
		throws IOException
	{
		return getEncoding(encoding).getTextWriter(out);
	} 

	
	
	public static boolean hasEncoding(String name)
	{
		return Charset.isSupported(name)
			|| Arrays.asList(ServiceManager.getServiceNames(serviceClass)).contains(name);
	} 

	
	private static final String serviceClass = "org.gjt.sp.jedit.io.Encoding";
	
}
