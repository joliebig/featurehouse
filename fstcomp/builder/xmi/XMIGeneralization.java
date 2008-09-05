package builder.xmi;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import de.ovgu.cide.fstgen.ast.FSTNode;

public class XMIGeneralization extends XMINonTerminal {
	
	public XMIGeneralization(Element node, Element root) {
		super("XMIGeneralization", node.getAttribute("name"), node, root);
		//Special class attributes
		for (XMIGeneralizationAttributes attribute : XMIGeneralizationAttributes.values()) {
			setNodeAttribute(attribute.toString(), node.getAttribute(attribute.toString()));
		}	
	}
	
	public void toFST() {
		NodeList children = getNode().getElementsByTagName("UML:Generalization.child");
		Element child = (Element)children.item(0);
		addChild(new XMIGenChild(child));
		
		NodeList parents = getNode().getElementsByTagName("UML:Generalization.parent");
		Element parent = (Element)parents.item(0);
		addChild(new XMIGenParent(parent));
	}
	
	@Override
	public Element toXMI(Document doc) {
		Element node = doc.createElement("UML:Generalization");
		//add special generalization node attributes
		for(XMIGeneralizationAttributes attribute : XMIGeneralizationAttributes.values()) {
			node.setAttribute(attribute.toString(), getNodeAttribute(attribute.toString()));
		}
		
		//node.setAttribute("xmi.id", "class"  + getNodeAttribute("name"));
		
		Element childNode = doc.createElement("UML:Generalization.child");
		Element parentNode = doc.createElement("UML:Generalization.parent");
		
		//add child and parent
		for(FSTNode fstNode : getChildren()) {
			if (fstNode instanceof XMIGenChild) {
				childNode.appendChild(((XMIGenChild)fstNode).toXMI(doc));
			} else if (fstNode instanceof XMIGenParent) {
				parentNode.appendChild(((XMIGenParent)fstNode).toXMI(doc));
			}
		}
		if (childNode.hasChildNodes()) {
			node.appendChild(childNode);
		}
		if (parentNode.hasChildNodes()) {
			node.appendChild(parentNode);
		}
		
		return node;
	}
	
	@Override
	public FSTNode getShallowClone() {
		return new XMIGeneralization(getNode(), getRoot());
	}

	@Override
	public FSTNode getDeepClone() {
		XMIGeneralization xmigen = new XMIGeneralization(getNode(), getRoot());
		xmigen.toFST();
		return xmigen;
	}
	
	private class XMIGenChild extends XMITerminal {
		
		Element node;
		
		XMIGenChild(Element node) {
			super("XMIGenChild", "");
			this.node = node;
			NodeList childClasses = node.getElementsByTagName("UML:Class");
			Element childClass = (Element)childClasses.item(0);
			setNodeAttribute("xmi.idref", childClass.getAttribute("xmi.idref"));
		}
		
		public Element toXMI(Document doc) {
			Element xmiclass = doc.createElement("UML:Class");
			xmiclass.setAttribute("xmi.idref", "class" + IdToElement(getNodeAttribute("xmi.idref"), "UML:Class"));
			return xmiclass;
		}	
		
		@Override
		public FSTNode getShallowClone() {
			return new XMIGenChild(node);
		}

		@Override
		public FSTNode getDeepClone() {
			return new XMIGenChild(node);
		}
	}
	
	private class XMIGenParent extends XMITerminal {
		
		Element node;
		
		XMIGenParent(Element node) {
			super("XMIGenParent", "");
			this.node = node;
			NodeList parentClasses = node.getElementsByTagName("UML:Class");
			Element parentClass = (Element)parentClasses.item(0);
			setNodeAttribute("xmi.idref", parentClass.getAttribute("xmi.idref"));
		}
		
		public Element toXMI(Document doc) {
			Element xmiclass = doc.createElement("UML:Class");
			xmiclass.setAttribute("xmi.idref", "class" + IdToElement(getNodeAttribute("xmi.idref"), "UML:Class"));
			return xmiclass;
		}
		
		@Override
		public FSTNode getShallowClone() {
			return new XMIGenParent(node);
		}

		@Override
		public FSTNode getDeepClone() {
			return new XMIGenParent(node);
		}
	}
}
