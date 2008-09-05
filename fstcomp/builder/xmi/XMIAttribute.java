package builder.xmi;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;


import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class XMIAttribute extends XMINonTerminal {

	public XMIAttribute(Element node, Element root) {
		super("XMIAttribute", node.getAttribute("name"), node, root);
		//Special class attributes
		for (XMIAttributeAttributes attribute : XMIAttributeAttributes.values()) {
			setNodeAttribute(attribute.toString(), node.getAttribute(attribute.toString()));
		}

		
	}
	
	public void toFST() {
		//get multiplicities
		NodeList multiplicities = getNode().getElementsByTagName("UML:MultiplicityRange");
		Element multiplicity = (Element)multiplicities.item(0);
		setNodeAttribute("lower", multiplicity.getAttribute("lower"));
		setNodeAttribute("upper", multiplicity.getAttribute("upper"));
		
		//get structural feature
		NodeList featureTypes = getNode().getElementsByTagName("UML:StructuralFeature.type");
		Element featureType = (Element)featureTypes.item(0);
		
		NodeList features = featureType.getElementsByTagName("UML:Class");
		if (features.getLength() > 0) {
			Element feature = (Element)features.item(0);
			addChild(new FSTTerminal("classRef", "class" + IdToElement(feature.getAttribute("xmi.idref"), "UML:Class"), "", ""));
		}
		
		features = featureType.getElementsByTagName("UML:DataType");
		if (features.getLength() > 0) {
			Element feature = (Element)features.item(0);
			//addChild(new XMIDataType(IdToElement(feature.getAttribute("xmi.idref"), "UML:DataType")));
			addChild(new FSTTerminal("dataTypeRef", "dataType" + IdToElement(feature.getAttribute("xmi.idref"), "UML:DataType"), "", ""));
		}
		
		features = featureType.getElementsByTagName("UML:Enumeration");
		if (features.getLength() > 0) {
			Element feature = (Element)features.item(0);
			//addChild(new XMIDataType(IdToElement(feature.getAttribute("xmi.idref"), "UML:DataType")));
			addChild(new FSTTerminal("enumRef", "enum" + IdToElement(feature.getAttribute("xmi.idref"), "UML:Enumeration"), "", ""));
		}
	}
	
	@Override
	public Element toXMI(Document doc) {
		Element node = doc.createElement("UML:Attribute");
		//add special attribute node attributes
		for(XMIAttributeAttributes attribute : XMIAttributeAttributes.values()) {
			node.setAttribute(attribute.toString(), getNodeAttribute(attribute.toString()));
		}
		
		//add multiplicity element
		Element mult = doc.createElement("UML:Multiplicity");
		Element structmult = doc.createElement("UML:StructuralFeature.multiplicity");
		Element multiplicity = doc.createElement("UML:Multiplicity.range");
		Element multiplicityRange = doc.createElement("UML:MultiplicityRange");
		multiplicityRange.setAttribute("lower", getNodeAttribute("lower"));
		multiplicityRange.setAttribute("upper", getNodeAttribute("upper"));
		multiplicity.appendChild(multiplicityRange);
		mult.appendChild(multiplicity);
		structmult.appendChild(mult);
		node.appendChild(structmult);
		
		//add structural feature
		Element dataType = null;
		Element structfeature = doc.createElement("UML:StructuralFeature.type");
		for (FSTNode fstNode : getChildren()) {
			String type = fstNode.getType();
			if (type.equals("classRef")) {
				dataType = doc.createElement("UML:Class");	
			} else if (type.equals("dataTypeRef")) {
				dataType = doc.createElement("UML:DataType");	
			} else if (type.equals("enumRef")) {
				dataType = doc.createElement("UML:Enumeration");
			}
			dataType.setAttribute("xmi.idref", fstNode.getName());
		}
		
		structfeature.appendChild(dataType);
		node.appendChild(structfeature);
		
		return node;
	}
	
	@Override
	public FSTNode getShallowClone() {
		XMIAttribute clone = new XMIAttribute(getNode(), getRoot());
		clone.toFST();
		return clone;
	}

	@Override
	public FSTNode getDeepClone() {
		XMIAttribute clone = new XMIAttribute(getNode(), getRoot());
		clone.toFST();
		return clone;
	}
}

