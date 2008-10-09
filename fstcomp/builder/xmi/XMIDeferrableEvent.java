package builder.xmi;


import org.w3c.dom.Document;
import org.w3c.dom.Element;
import de.ovgu.cide.fstgen.ast.FSTNode;

/**
 * An XMIDeferrableEvent can be used as trigger for states
 * or transitions. Possible types are: CallEvent, TimerEvent,
 * ChangeEvent and SignalEvent
 */
public class XMIDeferrableEvent extends XMITerminal {
	
	Element node;
	
	public XMIDeferrableEvent(Element node) {
		super("XMIDeferrable" + node.getNodeName(), node.getNodeName());
		this.node = node;
		setNodeAttribute("xmi.id", node.getAttribute("xmi.id"));
		setNodeAttribute("isSpecification", node.getAttribute("isSpecification"));
		setNodeAttribute("name", node.getAttribute("name"));
	}
	
	@Override
	public Element toXMI(Document doc) {
		Element node = doc.createElement(getName());
		node.setAttribute("xmi.id", getNodeAttribute("xmi.id"));
		node.setAttribute("name", getNodeAttribute("name"));
		node.setAttribute("isSpecification", getNodeAttribute("isSpecification"));
		return node;
	}
	
	@Override
	public FSTNode getShallowClone() {
		return new XMIDeferrableEvent(node);
	}

	@Override
	public FSTNode getDeepClone() {
		return new XMIDeferrableEvent(node);
	}
}
