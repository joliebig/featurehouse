

package org.gjt.sp.jedit.io;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.IOException;


public class BufferLocalEncodingDetector implements EncodingDetector
{
	public String detectEncoding(InputStream sample) throws IOException
	{
		BufferedReader reader
			= new BufferedReader(new InputStreamReader(sample));
		int i = 0;
		while (i < 10)
		{
			i++;
			String line = reader.readLine();
			if (line == null)
				return null;
			int pos = line.indexOf(":encoding=");
			if (pos != -1)
			{
				int p2 = line.indexOf(':', pos + 10);
				String encoding = line.substring(pos + 10, p2);
				return encoding;
			}
		}
		return null;
	}
}
