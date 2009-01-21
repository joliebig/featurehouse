
package org.jhotdraw.contrib.html; 
import java.io.InputStream; 
import java.io.Serializable; 
public  class  ResourceContentProducer  extends AbstractContentProducer 	implements Serializable {
		private String fResourceName;

		public ResourceContentProducer() { }

		public ResourceContentProducer(String resourceName) {	setResourceName(resourceName);	}

		public Object getContent(ContentProducerContext context, String ctxAttrName, Object ctxAttrValue) {	try {	String resourceName = (getResourceName() != null) ? getResourceName() : (String)ctxAttrValue;	InputStream reader = this.getClass().getResourceAsStream(resourceName);	int available = reader.available();	byte contents[] = new byte[available];	reader.read(contents, 0, available);	reader.close();	return new String(contents);	}	catch (Exception ex) {	ex.printStackTrace();	return ex.toString();	}	}

		public String getResourceName() {	return fResourceName;	}

		protected void setResourceName(String newResourceName) {	fResourceName = newResourceName;	}


}
