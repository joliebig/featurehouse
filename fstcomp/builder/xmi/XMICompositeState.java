package builder.xmi;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import de.ovgu.cide.fstgen.ast.FSTNode;

/**
 * A composite state can contain any kind of state
 *
 */
public class XMICompositeState extends XMINonTerminal {
	
	public XMICompositeState(Element node, Element root) {
		super("XMICompositeState", node.getAttribute("name"), node, root);
		for (XMICompositeStateAttributes attribute : XMICompositeStateAttributes.values()) {
			setNodeAttribute(attribute.toString(), node.getAttribute(attribute.toString()));
		}
	}
	
	public void toFST() {
		
		NodeList subvertexes = getNode().getElementsByTagName("UML:CompositeState.subvertex");
		if (subvertexes.getLength() > 0) {
			Node subvertex = subvertexes.item(0);
			NodeList nodes = subvertex.getChildNodes();
		
			for (int i = 0; i < nodes.getLength(); i ++) {
				Node node = nodes.item(i);
				String type = node.getNodeName();
			
				if (type.equals("UML:CompositeState")) {
					XMICompositeState xmicomp = new XMICompositeState((Element) node, getRoot());
					xmicomp.toFST();
					addChild(xmicomp);
				} else if (!type.equals("#text")) {
					XMIStateNode xmiState = new XMIStateNode(type, ((Element) node).getAttribute("name"), (Element) node, getRoot());
					xmiState.toFST();
					addChild(xmiState);
				}
			}
		}
	}

	@Override
	public Element toXMI(Document doc) {
		Element node = doc.createElement("UML:CompositeState");
		for (XMICompositeStateAttributes attribute : XMICompositeStateAttributes.values()) {
			node.setAttribute(attribute.toString(), getNodeAttribute(attribute.toString()));
		}
		
		Element subvertex = doc.createElement("UML:CompositeState.subvertex");
		for (FSTNode fstnode : getChildren()) {
			XMINode xminode = (XMINode) fstnode;
			subvertex.appendChild(xminode.toXMI(doc));
		}
		
		if (subvertex.hasChildNodes()) {
			node.appendChild(subvertex);
		}
		
		return node;
	}
	
	@Override
	public FSTNode getShallowClone() {
		return new XMICompositeState(getNode(), getRoot());
	}

	@Override
	public FSTNode getDeepClone() {
		XMICompositeState clone = new XMICompositeState(getNode(), getRoot());
		clone.toFST();
		return clone;
	}

}
