

package org.gjt.sp.jedit.io;


import java.io.InputStream;
import java.io.SequenceInputStream;
import java.io.ByteArrayInputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.io.IOException;
import java.nio.charset.UnsupportedCharsetException;
import java.nio.charset.MalformedInputException;
import java.util.Arrays;
import java.util.Map;
import java.util.HashMap;



public class EncodingWithBOM implements Encoding
{
	
	public EncodingWithBOM(String plain)
	{
		byte[] bom = bomMap.get(plain);
		if (bom == null)
		{
			throw new UnsupportedCharsetException(plain + " with BOM");
		}
		this.plain = new CharsetEncoding(plain);
		this.bom = bom;
	} 

	
	public Reader getTextReader(InputStream in) throws IOException
	{
		byte[] actualMark = new byte[bom.length];
		int count = in.read(actualMark);
		if (count < bom.length || !Arrays.equals(actualMark, bom))
		{
			throw new MalformedInputException(0);
		}
		return plain.getTextReader(in);
	}

	public Writer getTextWriter(OutputStream out) throws IOException
	{
		out.write(bom);
		return plain.getTextWriter(out);
	}

	public Reader getPermissiveTextReader(InputStream in) throws IOException
	{
		byte[] actualMark = new byte[bom.length];
		int count = in.read(actualMark);
		if (count < bom.length || !Arrays.equals(actualMark, bom))
		{
			
			
			
			in = new SequenceInputStream(
				new ByteArrayInputStream(actualMark, 0, count),
				in);
		}
		return plain.getPermissiveTextReader(in);
	}
	

	
	public static class Detector implements EncodingDetector
	{
		public String detectEncoding(InputStream sample) throws IOException
		{
			byte[] mark = new byte[4];
			int count = sample.read(mark);
	
			byte low = (byte)(BOM16 & 0xff);
			byte high = (byte)((BOM16 >> 8) & 0xff);
			if (count >= 4)
			{
				if (mark[0] == low && mark[1] == high
					&& mark[2] == 0x00 && mark[3] == 0x00)
				{
					return "X-UTF-32LE-BOM";
				}
				else if (mark[0] == 0x00 && mark[1] == 0x00
					&& mark[2] == high && mark[3] == low)
				{
					return "X-UTF-32BE-BOM";
				}
			}
			if (count >= 2)
			{
				if (mark[0] == low && mark[1] == high)
				{
					return "x-UTF-16LE-BOM";
				}
				else if (mark[0] == high && mark[1] == low)
				{
					
					
					
					
					return "UTF-16";
				}
			}
	
			if (count >= UTF8BOM.length)
			{
				int i = 0;
				while (i < UTF8BOM.length)
				{
					if (mark[i] != UTF8BOM[i])
					{
						break;
					}
					++i;
				}
				if (i == UTF8BOM.length)
				{
					return "UTF-8Y";
				}
			}
	
			return null;
		}
	} 

	

	
	private static final int BOM16 = 0xfeff;
	private static final byte[] UTF8BOM
		= { (byte)0xef, (byte)0xbb, (byte)0xbf };

	private static final Map<String, byte[]> bomMap
		= new HashMap<String, byte[]>();

	static
	{
		bomMap.put("UTF-8", UTF8BOM);

		byte low = (byte)(BOM16 & 0xff);
		byte high = (byte)((BOM16 >> 8) & 0xff);
		bomMap.put("UTF-16LE", new byte[] { low, high });
		bomMap.put("UTF-16BE", new byte[] { high, low });
		bomMap.put("UTF-32LE", new byte[] { low, high, 0x00, 0x00 });
		bomMap.put("UTF-32BE", new byte[] { 0x00, 0x00, high, low });
	}
	

	
	private final CharsetEncoding plain;
	private final byte[] bom;
	

	
}
