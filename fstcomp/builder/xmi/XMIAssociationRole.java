package builder.xmi;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import de.ovgu.cide.fstgen.ast.FSTNode;

public class XMIAssociationRole extends XMINonTerminal {
	
	private static Integer roleCounter = 0;
	
	public XMIAssociationRole(Element node, Element root) {
		super("XMIAssociationRole", roleCounter.toString(), node, root);
		for (XMIRoleAttributes roleAttr : XMIRoleAttributes.values()) {
			setNodeAttribute(roleAttr.toString(), node.getAttribute(roleAttr.toString()));
		}
		setNodeAttribute("xmi.id", node.getAttribute("xmi.id"));
		roleCounter++;
	}
	
	public void toFST() {
		
		//connection
		NodeList connections = getNode().getElementsByTagName("UML:Association.connection");
		if (connections.getLength() == 1) {
			Connection conn = new Connection((Element) connections.item(0), getRoot());
			conn.toFST();
			addChild(conn);
		} else {
			System.err.println("Undefined number of Association.connections!");
		}

		//message
		NodeList messages = getNode().getElementsByTagName("UML:AssociationRole.message");
		if (messages.getLength() == 1) {
			addChild(new Message((Element) messages.item(0), getRoot()));
		} else {
			System.err.println("Undefined numer of AssociationRole.messages!");
		}
		
		
	}

	@Override
	public Element toXMI(Document doc) {
		Element node = doc.createElement("UML:AssociationRole");
		for (XMIRoleAttributes roleAttr : XMIRoleAttributes.values()) {
			node.setAttribute(roleAttr.toString(), getNodeAttribute(roleAttr.toString()));
		}
		node.setAttribute("xmi.id", getNodeAttribute("xmi.id"));

		for (FSTNode fstNode : getChildren()) {
			node.appendChild(((XMINode) fstNode).toXMI(doc));
		}
		return node;
	}
	
	@Override
	public FSTNode getShallowClone() {
		return new XMIAssociationRole(getNode(), getRoot());
	}

	@Override
	public FSTNode getDeepClone() {
		XMIAssociationRole clone = new XMIAssociationRole(getNode(), getRoot());
		clone.toFST();
		return clone;
	}
	
	private class Connection extends XMINonTerminal {
		
		Connection(Element node, Element root) {
			super("XMIConnection", "", node, root);
		}
		
		void toFST() {
			NodeList endRoles = getNode().getElementsByTagName("UML:AssociationEndRole");
			for (int i = 0; i < endRoles.getLength(); i++) {
				EndRole endRole = new EndRole((Element) endRoles.item(i), getRoot());
				endRole.toFST();
				addChild(endRole);
			}
		}

		@Override
		public Element toXMI(Document doc) {
			Element node = doc.createElement("UML:Association.connection");
			for (FSTNode fstNode : getChildren()) {
				node.appendChild(((XMINode) fstNode).toXMI(doc));
			}
	
			return node;
		}
		
		@Override
		public FSTNode getShallowClone() {
			return new Connection(getNode(), getRoot());
		}

		@Override
		public FSTNode getDeepClone() {
			Connection clone = new Connection(getNode(), getRoot());
			clone.toFST();
			return clone;
		}
	}
	
	private class Message extends XMINonTerminal {
		
		Message(Element node, Element root) {
			super("XMIMessage", "", node, root);
			NodeList messages = getNode().getElementsByTagName("UML:Message");
			Element message = (Element) messages.item(0);
			setNodeAttribute("idref", "Message" + IdToElement(message.getAttribute("xmi.idref"),"UML:Message"));
		}
		
		@Override
		public Element toXMI(Document doc) {
			Element node = doc.createElement("UML:AssociationRole.message");
			Element message = doc.createElement("UML:Message");
			message.setAttribute("xmi.idref", getNodeAttribute("idref"));
			node.appendChild(message);
			return node;
		}
		
		@Override
		public FSTNode getShallowClone() {
			return new Message(getNode(), getRoot());
		}

		@Override
		public FSTNode getDeepClone() {
			return new Message(getNode(), getRoot());
		}
	}
	
	private class EndRole extends XMINonTerminal {
		
		Element node;
		
		EndRole(Element node, Element root) {
			super("XMIEndRole", "", node, root);
			this.node = node;
			setNodeAttribute("isSpecification", node.getAttribute("isSpecification"));
			setNodeAttribute("isNavigable", node.getAttribute("isNavigable"));
			setNodeAttribute("xmi.id", node.getAttribute("xmi.id"));
		}
		
		void toFST() {
			NodeList classifierRoles = node.getElementsByTagName("UML:ClassifierRole");
			Element classifierRole = (Element) classifierRoles.item(0);
			String idref = classifierRole.getAttribute("xmi.idref");
			setNodeAttribute("idref", "ClassifierRole"  + IdToElement(idref, "UML:ClassifierRole"));	
		}

		@Override
		public Element toXMI(Document doc) {
			Element endRole = doc.createElement("UML:AssociationEndRole");
			endRole.setAttribute("isSpecification", getNodeAttribute("isSpecification"));
			endRole.setAttribute("isNavigable", getNodeAttribute("isNavigable"));
			endRole.setAttribute("xmi.id", getNodeAttribute("xmi.id"));
			Element node = doc.createElement("UML:AssociationEnd.participant");
			Element role = doc.createElement("UML:ClassifierRole");
			role.setAttribute("xmi.idref", getNodeAttribute("idref"));
			node.appendChild(role);
			endRole.appendChild(node);
			return endRole;
		}
		
		@Override
		public FSTNode getShallowClone() {
			return new EndRole(getNode(), getRoot());
		}

		@Override
		public FSTNode getDeepClone() {
			Connection clone = new Connection(getNode(), getRoot());
			clone.toFST();
			return clone;
		}
		
	}
}
