package builder.xmi;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import de.ovgu.cide.fstgen.ast.FSTNode;

public class XMICollaboration extends XMINonTerminal {
	
	public XMICollaboration(Element node, Element root) {
		super("XMICollaboration", node.getAttribute("name"), node, root);
		
		if (node.getAttribute("name").equals("")) {
			System.out.println("Found a Collaboration without name!");
		}
		
		for (XMICollaborationAttributes collAttrib : XMICollaborationAttributes.values()) {
			setNodeAttribute(collAttrib.toString(), node.getAttribute(collAttrib.toString()));
		}
		
		setNodeAttribute("xmi.id", "Collaboration" + node.getAttribute("name"));
	}
	
	public void toFST() {
		// Namespace
		NodeList namespaces = getNode().getElementsByTagName("UML:Namespace.ownedElement");
		Element namespace = (Element) namespaces.item(0);
		NodeList classifierRoles = namespace.getElementsByTagName("UML:ClassifierRole");
		for (int i = 0; i < classifierRoles.getLength(); i++) {
			Element classifierRole = (Element) classifierRoles.item(i);
			if (classifierRole.getParentNode() == namespace) {
				addChild(new ClassifierRole(classifierRole, getRoot()));
			}
		}
		
		NodeList associationRoles = namespace.getElementsByTagName("UML:AssociationRole");
		for (int i = 0; i < associationRoles.getLength(); i++) {
			Element associationRole = (Element) associationRoles.item(i);
			if (associationRole.getParentNode() == namespace) {
				XMIAssociationRole xmiAssocRole = new XMIAssociationRole(associationRole, getRoot());
				xmiAssocRole.toFST();
				addChild(xmiAssocRole);
			}
		}
		
		// Interactions
		NodeList interactions = getNode().getElementsByTagName("UML:Collaboration.interaction");
		Element interaction = (Element) interactions.item(0);
		XMICollaborationInteraction collaboration = new XMICollaborationInteraction(interaction, getRoot());
		collaboration.toFST();
		addChild(collaboration);
	}

	@Override
	public Element toXMI(Document doc) {
		Element node = doc.createElement("UML:Collaboration");
		Element namespace = doc.createElement("UML:Namespace.ownedElement");
		
		for (XMICollaborationAttributes collAttrib : XMICollaborationAttributes.values()) {
			node.setAttribute(collAttrib.toString(), getNodeAttribute(collAttrib.toString()));
		}
		
		for (FSTNode fstNode : getChildren()) {
			if (fstNode instanceof ClassifierRole || fstNode instanceof XMIAssociationRole) {
				namespace.appendChild(((XMINode) fstNode).toXMI(doc));
			} else if (fstNode instanceof XMICollaborationInteraction) {
				node.appendChild(((XMINode) fstNode).toXMI(doc));
			}
		}
		
		node.setAttribute("xmi.id", getNodeAttribute("xmi.id"));
		node.appendChild(namespace);
		
		return node;
	}
	
	@Override
	public FSTNode getShallowClone() {
		return new XMICollaboration(getNode(), getRoot());
	}

	@Override
	public FSTNode getDeepClone() {
		XMICollaboration clone = new XMICollaboration(getNode(), getRoot());
		clone.toFST();
		return clone;
	}
	
	private class ClassifierRole extends XMINonTerminal {
		
		ClassifierRole(Element node, Element root) {
			super("XMIClassifierRole", node.getAttribute("name"), node, root);
			if (node.getAttribute("name").equals("")) {
				System.err.println("Found a ClassifierRole without name!");
			} else {
				setNodeAttribute("name", node.getAttribute("name"));
				setNodeAttribute("xmi.id", "ClassifierRole" + node.getAttribute("name"));
			}
			for (XMIRoleAttributes roleAttr : XMIRoleAttributes.values()) {
				setNodeAttribute(roleAttr.toString(), node.getAttribute(roleAttr.toString()));
			}
			
			NodeList multiplicities = node.getElementsByTagName("UML:MultiplicityRange");
			Element multiplicity = (Element) multiplicities.item(0);
			setNodeAttribute("lower", multiplicity.getAttribute("lower"));
			setNodeAttribute("upper", multiplicity.getAttribute("upper"));
			
		}

		@Override
		public Element toXMI(Document doc) {
			Element node = doc.createElement("UML:ClassifierRole");
			Element classMult = doc.createElement("UML:ClassifierRole.multiplicity");
			Element multiplicity = doc.createElement("UML:Multiplicity");
			Element multrange = doc.createElement("UML:Multiplicity.range");
			Element multRange = doc.createElement("UML:MultiplicityRange");
			
			node.setAttribute("xmi.id", getNodeAttribute("xmi.id"));
			node.setAttribute("name", getNodeAttribute("name"));
			for (XMIRoleAttributes roleAttr : XMIRoleAttributes.values()) {
				node.setAttribute(roleAttr.toString(), getNodeAttribute(roleAttr.toString()));
			}
			
			multRange.setAttribute("lower", getNodeAttribute("lower"));
			multRange.setAttribute("upper", getNodeAttribute("upper"));
			multrange.appendChild(multRange);
			multiplicity.appendChild(multrange);
			classMult.appendChild(multiplicity);
			node.appendChild(classMult);
			
			return node;
		}
		
		@Override
		public FSTNode getShallowClone() {
			return new ClassifierRole(getNode(), getRoot());
		}

		@Override
		public FSTNode getDeepClone() {
			ClassifierRole clone = new ClassifierRole(getNode(), getRoot());
			//clone.toFST();
			return clone;
		}
	}

}
