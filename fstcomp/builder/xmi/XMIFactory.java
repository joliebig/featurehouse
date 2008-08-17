package builder.xmi;

import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class XMIFactory {

	// XMI-document
	private Document xmi;
	// Content-root in XMI-document
	private Element root;
	// Root in FST
	private FSTNonTerminal FSTroot;
	// LinkManager
	private LinkManager linkManager = new LinkManager();
	

	
	List<FSTTerminal> associationClasses = new LinkedList<FSTTerminal>();
	List<FSTTerminal> associationEnums = new LinkedList<FSTTerminal>();
	List<FSTTerminal> dataTypes = new LinkedList<FSTTerminal>();

	public XMIFactory(File filename, FSTNonTerminal fstroot) {
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();

		try {
			DocumentBuilder db = dbf.newDocumentBuilder();
			xmi = db.parse(filename);
		} catch (ParserConfigurationException pce) {
			pce.printStackTrace();
		} catch (SAXException se) {
			se.printStackTrace();
		} catch (IOException ioe) {
			ioe.printStackTrace();
		}

		// Extract root node
		Element docEle = xmi.getDocumentElement();
		NodeList modelList = docEle
				.getElementsByTagName("UML:Namespace.ownedElement");

		FSTroot = fstroot;

		if (modelList.getLength() > 0) {
			root = (Element) modelList.item(0);
		}
	}

	/**
	 * Makes an FST out of the XMI-file
	 */
	public void extractFST() {

		// NodeList childNodes = root.getElementsByTagName("UML:Class");
		NodeList childNodes = root.getChildNodes();

		for (int i = 0; i < childNodes.getLength(); i++) {
			// Element node = (Element)childNodes.item(i);
			Node node = childNodes.item(i);

			// if (parent.equals("UML:Namespace.ownedElement")) {
			String nodeName = node.getNodeName();
			System.out.println(nodeName);
			if (nodeName.equals("UML:Class")) {
				FSTroot.addChild(extractClass((Element) node));
			} else if (nodeName.equals("UML:Association")) {
				FSTroot.addChild(extractAssociation((Element) node));
			} else if (nodeName.equals("UML:AssociationClass")) {
				FSTroot.addChild(extractClass((Element) node));
			} else if (nodeName.equals("UML:Generalization")) {
				FSTroot.addChild(extractGeneralization((Element) node));
			} else if (nodeName.equals("UML:Enumeration")) {
				FSTroot.addChild(extractEnumeration((Element) node));
			} else if (nodeName.equals("UML:Package")) {
				Element packageNode = (Element) node;
				FSTNonTerminal FSTpackage = new FSTNonTerminal("XMIPackage",
						packageNode.getAttribute("name"));

				FSTpackage.addChild(new FSTTerminal("isSpecification",
						packageNode.getAttribute("isSpecification"), "", ""));
				FSTpackage.addChild(new FSTTerminal("isRoot", packageNode
						.getAttribute("isRoot"), "", ""));
				FSTpackage.addChild(new FSTTerminal("isLeaf", packageNode
						.getAttribute("isLeaf"), "", ""));
				FSTpackage.addChild(new FSTTerminal("isAbstract", packageNode
						.getAttribute("isAbstract"), "", ""));

				NodeList packageNodes = packageNode
						.getElementsByTagName("UML:Package");
				for (int j = 0; j < packageNodes.getLength(); j++) {
					Element subPackage = (Element) packageNodes.item(j);
					extractPackage(subPackage);
				}
				FSTroot.addChild(FSTpackage);
			}
		}
		buildClassLinks();
		buildEnumLinks();
	}
	
	private FSTNonTerminal extractEnumeration(Element node) {
		FSTNonTerminal FSTenum = new FSTNonTerminal("XMIEnumeration", node.getAttribute("name"));
		FSTenum.addChild(new FSTTerminal("isSpecification", node
				.getAttribute("isSpecification"), "", ""));
		FSTenum.addChild(new FSTTerminal("isRoot", node
				.getAttribute("isRoot"), "", ""));
		FSTenum.addChild(new FSTTerminal("isLeaf", node
				.getAttribute("isLeaf"), "", ""));
		FSTenum.addChild(new FSTTerminal("isAbstract", node
				.getAttribute("isAbstract"), "", ""));
	
		//enumMap.put(node.getAttribute("xmi.id"), node.getAttribute("name"));
		linkManager.addEnum(node.getAttribute("xmi.id"), node.getAttribute("name"));
		
		NodeList enumerations = node.getElementsByTagName("UML:EnumerationLiteral");
		for (int i = 0; i < enumerations.getLength(); i++) {
			Element enumeration = (Element)enumerations.item(i);
			FSTNonTerminal FSTliteral = new FSTNonTerminal("Literal", enumeration.getAttribute("name"));
			FSTliteral.addChild(new FSTTerminal("isSpecification", enumeration.getAttribute("isSpecification"), "", ""));
			FSTenum.addChild(FSTliteral);
		}
		return FSTenum;
	}
	
	private FSTNonTerminal extractGeneralization(Element node) {
		FSTNonTerminal FSTgeneral = new FSTNonTerminal("XMIGeneralization","");
		FSTgeneral.addChild(new FSTTerminal("isSpecification",node.getAttribute("isSpecification"),"",""));
		
		NodeList children = node.getElementsByTagName("UML:Generalization.child");
		Element child = (Element)children.item(0);
		NodeList childClasses = child.getElementsByTagName("UML:Class");
		Element childClass = (Element)childClasses.item(0);
		
		NodeList parents = node.getElementsByTagName("UML:Generalization.parent");
		Element parent = (Element)parents.item(0);
		NodeList parentClasses = parent.getElementsByTagName("UML:Class");
		Element parentClass = (Element)parentClasses.item(0);
		
		FSTTerminal FSTchildClass = new FSTTerminal("Child",childClass.getAttribute("xmi.idref"),"","");
		FSTTerminal FSTparentClass = new FSTTerminal("Parent",parentClass.getAttribute("xmi.idref"),"","");
		associationClasses.add(FSTchildClass);
		associationClasses.add(FSTparentClass);
		
		FSTgeneral.addChild(FSTchildClass);
		FSTgeneral.addChild(FSTparentClass);
		return FSTgeneral;
	}
	
	private void extractPackage(Element node) {
		/*
		 * FSTNonTerminal FSTpackage = new FSTNonTerminal("XMIPacket", "");
		 * 
		 * FSTNonTerminal FSTpacketDetails = new
		 * FSTNonTerminal("XMIPacketDetails", node.getAttribute("name"));
		 * FSTpacketDetails.addChild(new FSTTerminal("isSpecification",
		 * node.getAttribute("isSpecification"), "", ""));
		 * FSTpacketDetails.addChild(new FSTTerminal("isRoot",
		 * node.getAttribute("isRoot"), "", "")); FSTpacketDetails.addChild(new
		 * FSTTerminal("isLeaf", node.getAttribute("isLeaf"), "", ""));
		 * FSTpacketDetails.addChild(new FSTTerminal("isAbstract",
		 * node.getAttribute("isAbstract"), "", ""));
		 * FSTpackage.addChild(FSTpacketDetails);
		 */

		NodeList classes = node.getElementsByTagName("UML:Class");
		for (int j = 0; j < classes.getLength(); j++) {
			Element XMIclass = (Element) classes.item(j);
			FSTroot.addChild(extractClass(XMIclass));
			/*
			 * FSTNonTerminal FSTclass = new FSTNonTerminal("XMIClass",
			 * XMIclass.getAttribute("name")); FSTclass.addChild(new
			 * FSTTerminal("isSpecification",
			 * XMIclass.getAttribute("isSpecification"), "", ""));
			 * FSTclass.addChild(new FSTTerminal("isRoot",
			 * XMIclass.getAttribute("isRoot"), "", "")); FSTclass.addChild(new
			 * FSTTerminal("isLeaf", XMIclass.getAttribute("isLeaf"), "", ""));
			 * FSTclass.addChild(new FSTTerminal("isAbstract",
			 * XMIclass.getAttribute("isAbstract"), "", ""));
			 * FSTclass.addChild(new FSTTerminal("isActive",
			 * XMIclass.getAttribute("isActive"), "", ""));
			 * FSTpackage.addChild(FSTclass);
			 */
		}
		// return null;
	}

	/**
	 * Transforms an XMI association node to a FSTNonTerminal
	 * 
	 * @param node
	 *            XMI node containing an association
	 * @return FSTNonTerminal containing an association
	 */
	private FSTNonTerminal extractAssociation(Element node) {
		FSTNonTerminal FSTassociation = new FSTNonTerminal("XMIAssociation",
				node.getAttribute("name"));
		FSTNonTerminal FSTassociationDetails = new FSTNonTerminal(
				"XMIAssociationDetails", "");
		FSTassociationDetails.addChild(new FSTTerminal("isSpecification", node
				.getAttribute("isSpecification"), "", ""));
		FSTassociationDetails.addChild(new FSTTerminal("isRoot", node
				.getAttribute("isRoot"), "", ""));
		FSTassociationDetails.addChild(new FSTTerminal("isLeaf", node
				.getAttribute("isLeaf"), "", ""));
		FSTassociationDetails.addChild(new FSTTerminal("isAbstract", node
				.getAttribute("isAbstract"), "", ""));
		FSTassociation.addChild(FSTassociationDetails);
		NodeList associations = node
				.getElementsByTagName("UML:Association.connection");
		Element association = (Element) associations.item(0);
		NodeList associationEnds = association
				.getElementsByTagName("UML:AssociationEnd");

		for (Integer j = 0; j < associationEnds.getLength(); j++) {
			FSTNonTerminal FSTassocEnd = new FSTNonTerminal("XMIAssocEnd", j
					.toString());
			Element associationEnd = (Element) associationEnds.item(j);
		
			NodeList multiplicityRanges = associationEnd
					.getElementsByTagName("UML:MultiplicityRange");
			Element multiplicityRange = (Element) multiplicityRanges.item(0);

			NodeList classes = associationEnd.getElementsByTagName("UML:Class");
			if (classes.getLength() > 0) {
				Element umlClass = (Element) classes.item(0);
				String classRefName = linkManager.getClassName(umlClass.getAttribute("xmi.idref")); // classMap.get(umlClass.getAttribute("xmi.idref"));
				FSTassocEnd.addChild(new FSTTerminal("xmi.idref", classRefName, "", ""));
			}
			
				

			FSTassocEnd.addChild(new FSTTerminal("lower", multiplicityRange
					.getAttribute("lower"), "", ""));
			FSTassocEnd.addChild(new FSTTerminal("upper", multiplicityRange
					.getAttribute("upper"), "", ""));
			FSTassocEnd.addChild(new FSTTerminal("isSpecification",
					associationEnd.getAttribute("isSpecification"), "", ""));
			FSTassocEnd.addChild(new FSTTerminal("visibility", associationEnd
					.getAttribute("visibility"), "", ""));
			FSTassocEnd.addChild(new FSTTerminal("isNavigable", associationEnd
					.getAttribute("isNavigable"), "", ""));
			FSTassocEnd.addChild(new FSTTerminal("ordering", associationEnd
					.getAttribute("ordering"), "", ""));
			FSTassocEnd.addChild(new FSTTerminal("aggregation", associationEnd
					.getAttribute("aggregation"), "", ""));
			FSTassocEnd.addChild(new FSTTerminal("targetScope", associationEnd
					.getAttribute("targetScope"), "", ""));
			FSTassocEnd.addChild(new FSTTerminal("changeability",
					associationEnd.getAttribute("changeability"), "", ""));
			
			FSTassociation.addChild(FSTassocEnd);
		}
		
		return FSTassociation;
	}

	/**
	 * Transforms an XMI node to a FSTNonTerminal
	 * 
	 * @param node
	 *            XMI node containing a class
	 * @return FSTNonTerminal containing a class
	 */
	private FSTNonTerminal extractClass(Element node) {

		FSTNonTerminal classNode = new FSTNonTerminal("XMIClass", node
				.getAttribute("name"));
		classNode.addChild(new FSTTerminal("xmi.id", node
				.getAttribute("xmi.id"), "", ""));
		classNode.addChild(new FSTTerminal("isAbstract", node
				.getAttribute("isAbstract"), "", ""));
		classNode.addChild(new FSTTerminal("isActive", node
				.getAttribute("isActive"), "", ""));
		classNode.addChild(new FSTTerminal("isLeaf", node
				.getAttribute("isLeaf"), "", ""));
		classNode.addChild(new FSTTerminal("isRoot", node
				.getAttribute("isRoot"), "", ""));
		classNode.addChild(new FSTTerminal("isSpecification", node
				.getAttribute("isSpecification"), "", ""));

		//classMap.put(node.getAttribute("xmi.id"), node.getAttribute("name"));
		linkManager.addClass(node.getAttribute("xmi.id"), node.getAttribute("name"));

		// add attributes as NonTerminals
		NodeList attributes = node.getElementsByTagName("UML:Attribute");
		for (int j = 0; j < attributes.getLength(); j++) {
			Element attribute = (Element) attributes.item(j);
			classNode.addChild(extractClassAttribute(attribute));
		}

		// add operations as NonTerminals
		NodeList operations = node.getElementsByTagName("UML:Operation");
		for (int j = 0; j < operations.getLength(); j++) {
			Element operation = (Element) operations.item(j);
			classNode.addChild(extractClassOperation(operation));
		}
		
		//AssociationClass?
		//TODO
		NodeList associations = node.getElementsByTagName("UML:Association.connection");
		if (associations.getLength() > 0) {
			classNode.addChild(extractAssociation(node));
		}

		return classNode;
	}

	/**
	 * Transforms an XMI attribtute node to a FSTNonTerminal attribute node
	 * 
	 * @param node
	 *            XMI node containing an attribute
	 * @return FSTNonTerminal containing an attribute
	 */
	private FSTNonTerminal extractClassAttribute(Element attribute) {
		FSTNonTerminal FSTAttribute = new FSTNonTerminal("XMIAttribute",
				attribute.getAttribute("name"));

		FSTAttribute.addChild(new FSTTerminal("xmi.id", attribute
				.getAttribute("xmi.id"), "", ""));
		FSTAttribute.addChild(new FSTTerminal("visibility", attribute
				.getAttribute("visibility"), "", ""));
		FSTAttribute.addChild(new FSTTerminal("isSpecification", attribute
				.getAttribute("isSpecification"), "", ""));
		FSTAttribute.addChild(new FSTTerminal("ownerScope", attribute
				.getAttribute("ownerScope"), "", ""));
		FSTAttribute.addChild(new FSTTerminal("changeability", attribute
				.getAttribute("changeability"), "", ""));
		FSTAttribute.addChild(new FSTTerminal("targetScope", attribute
				.getAttribute("targetScope"), "", ""));
		
		//Multiplicity
		NodeList multiplicities = attribute.getElementsByTagName("UML:MultiplicityRange");
		Element multiplicity = (Element)multiplicities.item(0);
		FSTAttribute.addChild(new FSTTerminal("lower", multiplicity
				.getAttribute("lower"), "", ""));
		FSTAttribute.addChild(new FSTTerminal("upper", multiplicity
				.getAttribute("upper"), "", ""));


		// extract link to DataType
		NodeList featureTypes = attribute
				.getElementsByTagName("UML:StructuralFeature.type");
		Element featureType = (Element) featureTypes.item(0);

		// UML-DataType-Feature found?
		NodeList dataTypes = featureType.getElementsByTagName("UML:DataType");
		if (dataTypes.getLength() > 0) {
			Element dataType = (Element) dataTypes.item(0);
			String idRef = dataType.getAttribute("xmi.idref");
			appendDataType(idRef, FSTAttribute, "UML:DataType");
		}

		// UML-Class-Feature found?
		dataTypes = featureType.getElementsByTagName("UML:Class");
		if (dataTypes.getLength() > 0) {
			Element dataType = (Element) dataTypes.item(0);
			String idRef = dataType.getAttribute("xmi.idref");
			appendDataType(idRef, FSTAttribute, "UML:Class");
		}
		
		// UML-Enumeration-Feature found?
		dataTypes = featureType.getElementsByTagName("UML:Enumeration");
		if (dataTypes.getLength() > 0) {
			Element dataType = (Element) dataTypes.item(0);
			String idRef = dataType.getAttribute("xmi.idref");
			FSTTerminal FSTenum = new FSTTerminal("XMIEnumeration", idRef, "", "");
			FSTAttribute.addChild(FSTenum);
			associationEnums.add(FSTenum);
			//appendDataType(idRef, FSTAttribute, "UML:Enumeration");
		}

		return FSTAttribute;
	}

	/**
	 * Adds a DataType-Node to the XMI-Content node
	 * 
	 * @param id
	 *            xmi.id
	 * @param FSTroot
	 *            the content root of the XMI-Document
	 * @param rootType
	 *            UML:Class or UML:DataType
	 */
	private void appendDataType(String id, FSTNonTerminal FSTroot,
			String rootType) {

		NodeList dataTypes = root.getElementsByTagName(rootType);
		for (int i = 0; i < dataTypes.getLength(); i++) {
			Element dataType = (Element) dataTypes.item(i);
			if (dataType.getAttribute("xmi.id").equals(id)) {
				FSTNonTerminal FSTdataType = new FSTNonTerminal("XMIDataType","");
				
				FSTdataType.addChild(new FSTTerminal("name",
						dataType.getAttribute("name"), "", ""));
				FSTdataType.addChild(new FSTTerminal("isSpecification",
						dataType.getAttribute("isSpecification"), "", ""));
				FSTdataType.addChild(new FSTTerminal("isRoot", dataType
						.getAttribute("isRoot"), "", ""));
				FSTdataType.addChild(new FSTTerminal("isLeaf", dataType
						.getAttribute("isLeaf"), "", ""));
				FSTdataType.addChild(new FSTTerminal("isAbstract", dataType
						.getAttribute("isAbstract"), "", ""));
				FSTroot.addChild(FSTdataType);
			}
		}
	}

	/**
	 * Transforms an XMI operation node to a FSTNonTerminal operation node
	 * 
	 * @param node
	 *            XMI node containing an operation
	 * @return FSTNonTerminal containing an operation
	 */
	private FSTNonTerminal extractClassOperation(Element operation) {
		FSTNonTerminal FSToperation = new FSTNonTerminal("XMIOperation",
				operation.getAttribute("name"));
		FSToperation.addChild(new FSTTerminal("isSpecification", operation
				.getAttribute("isSpecification"), "", ""));
		FSToperation.addChild(new FSTTerminal("ownerScope", operation
				.getAttribute("ownerScope"), "", ""));
		FSToperation.addChild(new FSTTerminal("isQuery", operation
				.getAttribute("isQuery"), "", ""));
		FSToperation.addChild(new FSTTerminal("concurrency", operation
				.getAttribute("concurrency"), "", ""));
		FSToperation.addChild(new FSTTerminal("isRoot", operation
				.getAttribute("isRoot"), "", ""));
		FSToperation.addChild(new FSTTerminal("isLeaf", operation
				.getAttribute("isLeaf"), "", ""));
		FSToperation.addChild(new FSTTerminal("isAbstract", operation
				.getAttribute("isAbstract"), "", ""));

		NodeList parameters = operation.getElementsByTagName("UML:Parameter");
		Element parameter = (Element) parameters.item(0);

		FSTNonTerminal FSTparameter = new FSTNonTerminal("XMIOperationParam",
				parameter.getAttribute("name"));
		FSTparameter.addChild(new FSTTerminal("isSpecification", parameter
				.getAttribute("isSpecification"), "", ""));
		FSTparameter.addChild(new FSTTerminal("kind", parameter
				.getAttribute("kind"), "", ""));

		NodeList dataTypes = parameter.getElementsByTagName("UML:DataType");
		NodeList classes = parameter.getElementsByTagName("UML:Class");

		if (dataTypes.getLength() > 0) {
			Element dataType = (Element) dataTypes.item(0);
			FSTparameter.addChild(new FSTTerminal("DataType", dataType
					.getAttribute("xmi.idref"), "", ""));
			FSToperation.addChild(FSTparameter);
			appendDataType(dataType.getAttribute("xmi.idref"), FSToperation,
					"UML:DataType");

		} else if (classes.getLength() > 0) {
			Element UMLclass = (Element) classes.item(0);
			// FSTparameter.addChild(new FSTTerminal("Class",
			// UMLclass.getAttribute("xmi.idref"), "", ""));
			FSTTerminal classLink = new FSTTerminal("Class", UMLclass
					.getAttribute("xmi.idref"), "", "");
			associationClasses.add(classLink);
			FSTparameter.addChild(classLink);
			FSToperation.addChild(FSTparameter);
		}

		return FSToperation;
	}

	private void buildClassLinks() {
		for (FSTTerminal classLink : associationClasses) {
			String idRef = classLink.getName();
			classLink.setName(linkManager.getClassName(idRef));
		}
	}
	
	private void buildEnumLinks() {
		for (FSTTerminal enumLink : associationEnums) {
			String idRef = enumLink.getName();
			enumLink.setName(linkManager.getEnumName(idRef));	
		}
	}
}
