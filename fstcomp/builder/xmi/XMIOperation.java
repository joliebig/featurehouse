package builder.xmi;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class XMIOperation extends XMINonTerminal {
		
	public XMIOperation(Element node, Element root ) {
		super("XMIOperation", node.getAttribute("name"), node, root);
		//Special operation attributes
		for (XMIOperationAttributes attribute : XMIOperationAttributes.values()) {
			setNodeAttribute(attribute.toString(), node.getAttribute(attribute.toString()));
		}
		
		
	}
	
	public void toFST() {
		//get behavorial feature parameters
		NodeList parameters = getNode().getElementsByTagName("UML:Parameter");
		Element parameter = (Element)parameters.item(0);
		setNodeAttribute("returnName", parameter.getAttribute("name"));
		setNodeAttribute("returnisSpec", parameter.getAttribute("isSpecification"));
		setNodeAttribute("returnKind", parameter.getAttribute("kind"));
		
		//get parameter type
		NodeList featureTypes = getNode().getElementsByTagName("UML:Parameter.type");
		Element featureType = (Element)featureTypes.item(0);
		
		NodeList features = featureType.getElementsByTagName("UML:Class");
		if (features.getLength() > 0) {
			Element feature = (Element)features.item(0);
			addChild(new FSTTerminal("classRef", "class" + IdToElement(feature.getAttribute("xmi.idref"), "UML:Class"), "", ""));
		}
		
		features = featureType.getElementsByTagName("UML:DataType");
		if (features.getLength() > 0) {
			Element feature = (Element)features.item(0);
			addChild(new FSTTerminal("dataTypeRef", "dataType" + IdToElement(feature.getAttribute("xmi.idref"), "UML:DataType"), "", ""));
		}
		
		features = featureType.getElementsByTagName("UML:Enumeration");
		if (features.getLength() > 0) {
			Element feature = (Element)features.item(0);
			addChild(new FSTTerminal("enumRef", "enum" + IdToElement(feature.getAttribute("xmi.idref"), "UML:Enumeration"), "", ""));
		}	
	}
	
	@Override
	public Element toXMI(Document doc) {
		Element node = doc.createElement("UML:Operation");

		//add special operation node attributes
		for(XMIOperationAttributes attribute : XMIOperationAttributes.values()) {
			node.setAttribute(attribute.toString(), getNodeAttribute(attribute.toString()));
		}
		
		//add behavioral feature element
		Element behavioralFeature = doc.createElement("UML:BehavioralFeature.parameter");
		Element parameter = doc.createElement("UML:Parameter");
		Element parameterType = doc.createElement("UML:Parameter.type");
		parameter.setAttribute("name", getNodeAttribute("returnName"));
		parameter.setAttribute("isSpecification", getNodeAttribute("returnisSpec"));
		parameter.setAttribute("kind", getNodeAttribute("returnKind"));
		
		//add parameter type
		Element dataType = null;
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
		
		parameterType.appendChild(dataType);
		parameter.appendChild(parameterType);
		behavioralFeature.appendChild(parameter);
		node.appendChild(behavioralFeature);
		
		return node;
	}
	
	@Override
	public FSTNode getShallowClone() {
		return new XMIOperation(getNode(), getRoot());
	}

	@Override
	public FSTNode getDeepClone() {
		XMIOperation clone = new XMIOperation(getNode(), getRoot());
		clone.toFST();
		return clone;
	}
}
