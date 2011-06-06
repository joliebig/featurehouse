

package org.gjt.sp.jedit.io;


import java.io.InputStream;
import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.Reader;
import java.io.InputStreamReader;
import java.io.Writer;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;
import java.util.List;
import java.util.ArrayList;

import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.ServiceManager;
import org.gjt.sp.jedit.bufferio.BufferIORequest;
import org.gjt.sp.jedit.MiscUtilities;
import org.gjt.sp.util.Log;



public class AutoDetection
{
	
	
	public static BufferedInputStream getMarkedStream(InputStream in)
	{
		int bufferSize = BufferIORequest.getByteIOBufferSize();
		BufferedInputStream markable
			= new BufferedInputStream(in, bufferSize);
		assert(markable.markSupported());
		markable.mark(bufferSize);
		return markable;
	} 

	
	
	public static boolean isGzipped(InputStream sample)
		throws IOException
	{
		final byte[] GZIP_MAGIC = { (byte)0x1f, (byte)0x8b };
		return sample.read() == GZIP_MAGIC[0]
			&& sample.read() == GZIP_MAGIC[1];
	} 

	
	
	public static List<EncodingDetector> getEncodingDetectors()
	{
		List<EncodingDetector> detectors
			= new ArrayList<EncodingDetector>();
		String propName = "encodingDetectors";
		String selectedDetectors
			= jEdit.getProperty(propName, "BOM XML-PI");
		if (selectedDetectors != null
			&& selectedDetectors.length() > 0)
		{
			for (String name: selectedDetectors.split("\\s+"))
			{
				EncodingDetector service
					= getEncodingDetectorService(name);
				if (service != null)
				{
					detectors.add(service);
				}
				else
				{
					Log.log(Log.ERROR, AutoDetection.class
						, "getEncodingDetectors():"
							+ " No EncodingDetector for the name"
							+ " \"" + name + "\"");
				}
			}
		}
		return detectors;
	} 

	
	
	public static String getDetectedEncoding(BufferedInputStream markedStream)
		throws IOException
	{
		List<EncodingDetector> detectors = getEncodingDetectors();
		for (EncodingDetector detector: detectors)
		{
			
			
			
			markedStream.reset();
			
			
			
			String detected = detector.detectEncoding(
				new BufferedInputStream(markedStream));
			if (detected != null)
			{
				return detected;
			}
		}
		return null;
	} 

	
	
	private static EncodingDetector getEncodingDetectorService(String name)
	{
		String serviceClass = "org.gjt.sp.jedit.io.EncodingDetector";
		Object service = ServiceManager.getService(serviceClass, name);
		if (service != null && service instanceof EncodingDetector)
		{
			return (EncodingDetector)service;
		}
		else
		{
			return null;
		}
	}
	
}
