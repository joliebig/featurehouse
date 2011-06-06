

package org.gjt.sp.jedit.io;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.io.IOException;


public interface Encoding
{
	
	public Reader getTextReader(InputStream in) throws IOException;
	
	
	public Writer getTextWriter(OutputStream out) throws IOException;

	
	public Reader getPermissiveTextReader(InputStream in)
		throws IOException;
}
