package builder.xmi;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

public interface XMINode {
	Element toXMI(Document doc);
}
