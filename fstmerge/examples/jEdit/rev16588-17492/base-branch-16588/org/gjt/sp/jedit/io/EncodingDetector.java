

package org.gjt.sp.jedit.io;

import java.io.InputStream;
import java.io.IOException;


public interface EncodingDetector
{
	
	public String detectEncoding(InputStream sample) throws IOException;
}
