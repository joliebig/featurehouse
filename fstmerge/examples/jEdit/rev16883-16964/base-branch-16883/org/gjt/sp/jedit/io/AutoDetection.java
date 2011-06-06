

package org.gjt.sp.jedit.io;


import java.io.InputStream;
import java.io.BufferedInputStream;
import java.io.IOException;
import java.util.List;
import java.util.ArrayList;
import java.util.zip.GZIPInputStream;

import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.ServiceManager;
import org.gjt.sp.jedit.bufferio.BufferIORequest;
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
		int magic1 = GZIPInputStream.GZIP_MAGIC & 0xff;
		int magic2 = (GZIPInputStream.GZIP_MAGIC >> 8) & 0xff;
		return sample.read() == magic1
			&& sample.read() == magic2;
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

	
	
	public static class Result
	{
		
		
		public Result(InputStream in) throws IOException
		{
			BufferedInputStream marked = getMarkedStream(in);

			gzipped = isGzipped(marked);
			if (gzipped)
			{
				marked.reset();
				marked = getMarkedStream(
					new GZIPInputStream(marked));
			}

			marked.reset();
			encoding = AutoDetection.getDetectedEncoding(marked);

			markedStream = marked;
		} 

		
		
		public BufferedInputStream getRewindedStream()
			throws IOException
		{
			markedStream.reset();
			return markedStream;
		} 

		
		
		public boolean streamIsGzipped()
		{
			return gzipped;
		} 

		
		
		public String getDetectedEncoding()
		{
			return encoding;
		} 

		
		private final BufferedInputStream markedStream;
		private final boolean gzipped;
		private final String encoding;
		
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
