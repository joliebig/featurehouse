
package org.jhotdraw.contrib.html; 
import java.io.InputStream; 
import java.io.Serializable; 
import java.net.URL; 
public  class  URLContentProducer  extends FigureDataContentProducer  implements Serializable {
		private URL fURL;

		public URLContentProducer() { }

		public URLContentProducer(URL url) {	setURL(url);	}

		public Object getContent(ContentProducerContext context, String ctxAttrName, Object ctxAttrValue) {	try {	URL url = (getURL() != null) ? new URL(getURL().toExternalForm()) : new URL(((URL)ctxAttrValue).toExternalForm());	InputStream reader = url.openStream();	int available = reader.available();	byte contents[] = new byte[available];	reader.read(contents, 0, available);	reader.close();	return new String(contents);	}	catch (Exception ex) {	ex.printStackTrace();	return ex.toString();	}	}

		public URL getURL() {	return fURL;	}

		protected void setURL(URL newURL) {	fURL = newURL;	}


}
