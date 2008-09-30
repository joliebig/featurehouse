package builder.xmi;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import de.ovgu.cide.fstgen.ast.FSTNode;

public class XMISignalEvent extends XMITerminal {
	
	Element node;
	
	public XMISignalEvent(Element node) {
		super("XMISignalEvent", node.getAttribute("name"));
		this.node = node;
		setNodeAttribute("isSpecification", node.getAttribute("isSpecification"));
		setNodeAttribute("xmi.id", node.getAttribute("xmi.id"));
		setNodeAttribute("name", node.getAttribute("name"));
	}

	@Override
	public Element toXMI(Document doc) {
		Element node = doc.createElement("UML:SignalEvent");
		node.setAttribute("xmi.id", getNodeAttribute("xmi.id"));
		node.setAttribute("name", getNodeAttribute("name"));
		node.setAttribute("isSpecification", getNodeAttribute("isSpecification"));
		return node;
	}
	
	@Override
	public FSTNode getShallowClone() {
		return new XMISignalEvent(node);
	}

	@Override
	public FSTNode getDeepClone() {
		return new XMISignalEvent(node);
	}

}
