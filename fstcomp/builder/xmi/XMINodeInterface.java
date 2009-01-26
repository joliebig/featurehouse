package builder.xmi;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

public interface XMINodeInterface {
	Element toXMI(Document doc);
}
