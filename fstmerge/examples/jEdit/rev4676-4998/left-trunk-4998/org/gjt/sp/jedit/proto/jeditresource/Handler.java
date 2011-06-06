

package org.gjt.sp.jedit.proto.jeditresource;

import java.io.IOException;
import java.net.*;

public class Handler extends URLStreamHandler
{
	public URLConnection openConnection(URL url)
		throws IOException
	{
		PluginResURLConnection c = new PluginResURLConnection(url);
		c.connect();
		return c;
	}
}
