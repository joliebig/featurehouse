package builder.xmi;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;


import de.ovgu.cide.fstgen.ast.FSTNode;

public class XMIAssociation extends XMINonTerminal {

	
	public XMIAssociation(Element node, Element root)  {
		super("XMIAssoociation", node.getAttribute("name"), node, root);
		//Special association attributes
		for (XMIAssociationAttributes attribute : XMIAssociationAttributes.values()) {
			setNodeAttribute(attribute.toString(), node.getAttribute(attribute.toString()));
		}
	}
	
	public void toFST() {
		// add association ends as terminals
		NodeList ends = getNode().getElementsByTagName("UML:AssociationEnd");
		for (int j = 0; j < ends.getLength(); j++) {
			Element end = (Element) ends.item(j);
			addChild(new XMIAssociationEnd(end));
		}	
	}
	
	@Override
	public Element toXMI(Document doc) {
		Element node = doc.createElement("UML:Association");
		
		for(XMIAssociationAttributes attribute : XMIAssociationAttributes.values()) {
			node.setAttribute(attribute.toString(), getNodeAttribute(attribute.toString()));
		}
		
		//add association ends to "connection" node
		Element connection = doc.createElement("UML:Association.connection");
		for(FSTNode fstNode : getChildren()) {
			if (fstNode instanceof XMIAssociationEnd) {
				connection.appendChild(((XMIAssociationEnd) fstNode).toXMI(doc));
			}
		}
		
		node.appendChild(connection);
		return node;
	}
	
	@Override
	public FSTNode getShallowClone() {
		return new XMIAssociation(getNode(), getRoot());
	}

	@Override
	public FSTNode getDeepClone() {
		XMIAssociation clone = new XMIAssociation(getNode(), getRoot());
		clone.toFST();
		return clone;
	}
	
	
	class XMIAssociationEnd extends XMITerminal {
		
		private Element node;

		public XMIAssociationEnd(Element node) {
			super("XMIAssociationEnd", "");
			this.node = node;
			//Special association end attributes
			for (XMIAssociationEndAttributes attribute : XMIAssociationEndAttributes.values()) {
				setNodeAttribute(attribute.toString(), node.getAttribute(attribute.toString()));
			}
			
			//get multiplicities
			NodeList multRanges = node.getElementsByTagName("UML:MultiplicityRange");
			Element multRange = (Element)multRanges.item(0);
			setNodeAttribute("lower", multRange.getAttribute("lower"));
			setNodeAttribute("upper", multRange.getAttribute("upper"));
			
			//get referencing classes
			//assert class
			NodeList classes = node.getElementsByTagName("UML:Class");
			Element refClass = (Element)classes.item(0);
			setNodeAttribute("xmi.idref",IdToElement(refClass.getAttribute("xmi.idref"), "UML:Class"));
		}
		
		public Element toXMI(Document doc) {
			Element assocEnd = doc.createElement("UML:AssociationEnd");
			
			for(XMIAssociationEndAttributes attribute : XMIAssociationEndAttributes.values()) {
				assocEnd.setAttribute(attribute.toString(), getNodeAttribute(attribute.toString()));
			}
			
			Element assocEndmult = doc.createElement("UML:AssociationEnd.multiplicity");
			Element mult = doc.createElement("UML:Multiplicity");
			Element multrange = doc.createElement("UML:Multiplicity.range");
			Element multRange = doc.createElement("UML:MultiplicityRange");
			Element assocEndpart = doc.createElement("UML:AssociationEnd.participant");
			Element partClass = doc.createElement("UML:Class");
			
			multRange.setAttribute("lower", getNodeAttribute("lower"));
			multRange.setAttribute("upper", getNodeAttribute("upper"));
			partClass.setAttribute("xmi.idref", "class" + getNodeAttribute("xmi.idref"));
			
			//build UML:AssociationEnd.multiplicity tree
			multrange.appendChild(multRange);
			mult.appendChild(multrange);
			assocEndmult.appendChild(mult);
			assocEnd.appendChild(assocEndmult);
			
			//build UML:AssociationEnd.participant tree
			assocEndpart.appendChild(partClass);
			assocEnd.appendChild(assocEndpart);
			
			return assocEnd;
		}
		
		@Override
		public FSTNode getShallowClone() {
			return new XMIAssociationEnd(node);
		}

		@Override
		public FSTNode getDeepClone() {
			return new XMIAssociationEnd(node);
		}
		
	}

}
