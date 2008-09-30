package builder.xmi;


import org.w3c.dom.Document;
import org.w3c.dom.Element;
import de.ovgu.cide.fstgen.ast.FSTNode;

public class XMIStateAction extends XMITerminal {
	
	Element residual;
	
	public XMIStateAction(Element node) {
		super("XMIStateAction", node.getNodeName());
		setNodeAttribute("name", node.getNodeName());
		residual = node;
	}
		

	@Override
	public Element toXMI(Document doc) {
		return (Element) doc.importNode(residual, true);
	}
	
	@Override
	public FSTNode getShallowClone() {
		return new XMIStateAction(residual);
	}

	@Override
	public FSTNode getDeepClone() {
		return new XMIStateAction(residual);
	}

}
