package builder.xmi;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import de.ovgu.cide.fstgen.ast.FSTNode;

public class XMIEnumeration extends XMINonTerminal {
	
	public XMIEnumeration(Element node, Element root) {
		super("XMIEnumeration", node.getAttribute("name"), node, root);
		//Special class attributes
		for (XMIEnumerationAttributes attribute : XMIEnumerationAttributes.values()) {
			setNodeAttribute(attribute.toString(), node.getAttribute(attribute.toString()));
		}	
	}
	
	public void toFST() {
		// add association ends as nonterminals
		NodeList enumends = getNode().getElementsByTagName("UML:EnumerationLiteral");
		for (int j = 0; j < enumends.getLength(); j++) {
			Element enumend = (Element) enumends.item(j);
			addChild(new XMIEnumEnd(enumend));
		}	
	}
	
	@Override
	public Element toXMI(Document doc) {
		Element node = doc.createElement("UML:Enumeration");
		//add special enumeration node attributes
		for(XMIEnumerationAttributes attribute : XMIEnumerationAttributes.values()) {
			node.setAttribute(attribute.toString(), getNodeAttribute(attribute.toString()));
		}
		
		node.setAttribute("xmi.id", "enum"  + getNodeAttribute("name"));
		
		//enum node: here we attach the enumeration ends
		Element enumNode = doc.createElement("UML:Enumeration.literal");
		
		//add enumeration ends
		for(FSTNode fstNode : getChildren()) {
			if (fstNode instanceof XMIEnumEnd) {
				enumNode.appendChild(((XMIEnumEnd) fstNode).toXMI(doc));
			}		
		}
		
		if (enumNode.hasChildNodes()) {
			node.appendChild(enumNode);
		}
		
		return node;
	}
	
	@Override
	public FSTNode getShallowClone() {
		return new XMIEnumeration(getNode(), getRoot());
	}

	@Override
	public FSTNode getDeepClone() {
		XMIEnumeration clone = new XMIEnumeration(getNode(), getRoot());
		clone.toFST();
		return clone;
	}
	
	/**
	 * 
	 *
	 */
	private class XMIEnumEnd extends XMITerminal {
		
		Element node;
		
		XMIEnumEnd(Element node) {
			super("XMIEnumerationEnd", node.getAttribute("name"));
			this.node = node;
			setNodeAttribute("name", node.getAttribute("name"));
			setNodeAttribute("isSpecification", node.getAttribute("isSpecification"));
		}
		
		@Override
		public Element toXMI(Document doc) {
			Element enumEnd = doc.createElement("UML:EnumerationLiteral");
			enumEnd.setAttribute("name", getNodeAttribute("name"));
			enumEnd.setAttribute("isSpecification", getNodeAttribute("isSpecification"));
			return enumEnd;
		}
		
		@Override
		public FSTNode getShallowClone() {
			return new XMIEnumEnd(node);
		}

		@Override
		public FSTNode getDeepClone() {
			return new XMIEnumEnd(node);
		}
	}

}
