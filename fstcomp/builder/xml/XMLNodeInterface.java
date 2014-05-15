package builder.xml;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

public interface XMLNodeInterface {
	Element toXML(Document doc);
}
