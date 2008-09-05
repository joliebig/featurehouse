package builder.xmi;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;


import de.ovgu.cide.fstgen.ast.FSTNode;

public class XMIClass extends XMINonTerminal {
	
	public XMIClass(Element node, Element root)  {
		super("XMIClass", node.getAttribute("name"), node, root);
		//Special class attributes
		for (XMIClassAttributes attribute : XMIClassAttributes.values()) {
			setNodeAttribute(attribute.toString(), node.getAttribute(attribute.toString()));
		}		
	}
	
	public XMIClass(String type, Element node, Element root) {
		super(type, node.getAttribute("name"), node, root);
		//Special class attributes
		for (XMIClassAttributes attribute : XMIClassAttributes.values()) {
			setNodeAttribute(attribute.toString(), node.getAttribute(attribute.toString()));
		}	
	}
	
	public void toFST() {
		// add attributes as NonTerminals
		NodeList attributes = getNode().getElementsByTagName("UML:Attribute");
		for (int j = 0; j < attributes.getLength(); j++) {
			Element attribute = (Element) attributes.item(j);
			XMIAttribute xmiattrib = new XMIAttribute(attribute, getRoot());
			xmiattrib.toFST();
			addChild(xmiattrib);
		}	
		
		// add operations as NonTerminals
		NodeList operations = getNode().getElementsByTagName("UML:Operation");
		for (int j = 0; j < operations.getLength(); j++) {
			Element operation = (Element) operations.item(j);
			XMIOperation xmiop = new XMIOperation(operation, getRoot());
			xmiop.toFST();
			addChild(xmiop);
		}	
	}

	@Override
	public Element toXMI(Document doc) {
		Element node = doc.createElement("UML:Class");
		//add special class node attributes
		for(XMIClassAttributes attribute : XMIClassAttributes.values()) {
			node.setAttribute(attribute.toString(), getNodeAttribute(attribute.toString()));
		}
		
		node.setAttribute("xmi.id", "class"  + getNodeAttribute("name"));
		
		//feature node: here we attach attributes and operations
		Element featureNode = doc.createElement("UML:Classifier.feature");
		
		//add operations and attributes
		for(FSTNode fstNode : getChildren()) {
			if (fstNode instanceof XMIAttribute) {
				featureNode.appendChild(((XMIAttribute)fstNode).toXMI(doc));
			} else if (fstNode instanceof XMIOperation) {
				featureNode.appendChild(((XMIOperation)fstNode).toXMI(doc));
			}
		}
		if (featureNode.hasChildNodes()) {
			node.appendChild(featureNode);
		}
		
		return node;
	}

	@Override
	public FSTNode getShallowClone() {
		return new XMIClass(getNode(), getRoot());
	}

	@Override
	public FSTNode getDeepClone() {
		XMIClass clone = new XMIClass(getNode(), getRoot());
		clone.toFST();
		return clone;
	}
}
