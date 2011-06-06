

package org.gjt.sp.jedit.io;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.nio.CharBuffer;


public class RegexEncodingDetector implements EncodingDetector
{
	
	public static final String VALID_ENCODING_PATTERN
		= "\\p{Alnum}[\\p{Alnum}\\-.:_]*";

	private final Pattern pattern;
	private final String replacement;

	public RegexEncodingDetector(String pattern, String replacement)
	{
		this.pattern = Pattern.compile(pattern);
		this.replacement = replacement;
	}

	public String detectEncoding(InputStream sample) throws IOException
	{
		InputStreamReader reader = new InputStreamReader(sample);
		final int bufferSize = 1024;
		char[] buffer = new char[bufferSize];
		int readSize = reader.read(buffer, 0, bufferSize);
		if (readSize > 0)
		{
			Matcher matcher = pattern.matcher(
				CharBuffer.wrap(buffer, 0, readSize));

			
			
			
			int appendPosition = 0;

			while (matcher.find())
			{
				String extracted = extractReplacement(
					matcher, appendPosition, replacement);
				if (EncodingServer.hasEncoding(extracted))
				{
					return extracted;
				}
				appendPosition = matcher.end();
			}
		}
		return null;
	}

	
	private static String extractReplacement(
		Matcher found, int appendPosition, String replacement)
	{
		
		int found_start = found.start();
		int found_end = found.end();
		int source_length = found_end - found_start;
		int length_before_match = found_start - appendPosition;
		StringBuffer replaced = new StringBuffer(
				length_before_match + (source_length * 2));
		found.appendReplacement(replaced, replacement);
		return replaced.substring(length_before_match);
	}
}
