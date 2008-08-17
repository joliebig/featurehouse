package printer.xmi;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class XMIPrinter {

	FSTNode root;
	String filename;
	Document xmi = null;
	Element documentRoot;
	Integer uniqueID = 0;

	List<Association> associations = new LinkedList<Association>();
	LinkManager linkManager = new LinkManager();

	public XMIPrinter(FSTNode root, String filename) {
		this.root = root;
		this.filename = filename;
	}

	/**
	 * Prints the result into an new XMI-file
	 * 
	 * @param filename
	 */
	public void transformDocument() {
		Transformer transformer;

		documentRoot = createRoot();
		process();
		linkManager.createDataTypes(documentRoot, xmi);
		//Create Associations
		for (Association association : associations) {
			association.createXMI(documentRoot, xmi, linkManager);
		}
		linkManager.createClassLinks();
		linkManager.createEnumLinks();
		try {

			transformer = TransformerFactory.newInstance().newTransformer();

			DOMSource source = new DOMSource(xmi);
			FileOutputStream os = new FileOutputStream(filename);
			StreamResult result = new StreamResult(os);
			transformer.transform(source, result);
		} catch (TransformerConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (TransformerFactoryConfigurationError e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (TransformerException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	private void process() {
		FSTNonTerminal nonterminal = (FSTNonTerminal) root;
		for (FSTNode child : nonterminal.getChildren()) {

			if (child.getType().equals("XMIClass")) {
				// temp storage of attributes and operations
				Element featureNodes = xmi
						.createElement("UML:Classifier.feature");

				// Classnode
				Element classNode = xmi.createElement("UML:Class");
				Element assocClassNode = xmi.createElement("UML:AssociationClass");
				classNode.setAttribute("name", child.getName());
				assocClassNode.setAttribute("name", child.getName());

				FSTNonTerminal FSTClass = (FSTNonTerminal) child;
				boolean isAssocClass = false;
				for (FSTNode FSTchild : FSTClass.getChildren()) {
					
					String type = FSTchild.getType();

					if (type.equals("XMIAttribute")) {
						featureNodes
								.appendChild(createAttribute((FSTNonTerminal) FSTchild));
					} else if (type.equals("XMIOperation")) {
						featureNodes
								.appendChild(createOperation((FSTNonTerminal) FSTchild));
					} else if (type.equals("XMIAssociation")) {
						associations.add(new Association(FSTchild, classNode));
						associations.add(new Association(FSTchild, assocClassNode));
						isAssocClass = true;
					} else {
						classNode.setAttribute(type, FSTchild.getName());
						assocClassNode.setAttribute(type, FSTchild.getName());
					}

				}
				
				if (featureNodes.hasChildNodes()) {
					if (isAssocClass) {
						assocClassNode.appendChild(featureNodes);
					} else {
						classNode.appendChild(featureNodes);
					}
				}
				
				// Save name and ID refMap for later reconstruction of
				// associations
				String IDname = classNode.getAttribute("name");
				String newID = createUniqueID();
				linkManager.addClass(IDname, newID);
				
				if (isAssocClass) {
					assocClassNode.setAttribute("xmi.id", newID);
					documentRoot.appendChild(assocClassNode);
				} else {
					classNode.setAttribute("xmi.id", newID);
					documentRoot.appendChild(classNode);
				}
				
			} else if (child.getType().equals("XMIAssociation")) {
				associations.add(new Association(child));
			} else if (child.getType().equals("XMIGeneralization")) {
				documentRoot.appendChild(createGeneralization((FSTNonTerminal)child));
			} else if (child.getType().equals("XMIEnumeration")) {
				documentRoot.appendChild(createEnumeration((FSTNonTerminal)child));
			}
		}
	}
	
	private Element createEnumeration(FSTNonTerminal FSTchild) {
		Element enumeration = xmi.createElement("UML:Enumeration");
		enumeration.setAttribute("name", FSTchild.getName());
		Element enumliteral = xmi.createElement("UML:Enumeration.literal");
		enumeration.appendChild(enumliteral);
		
		String xmiid = createUniqueID();
		enumeration.setAttribute("xmi.id", xmiid);
		linkManager.addEnum(FSTchild.getName(), xmiid);
		
		for (FSTNode FSTnode : FSTchild.getChildren()) {
			if (FSTnode instanceof FSTTerminal) {
				FSTTerminal FSTterminal = (FSTTerminal)FSTnode;
				enumeration.setAttribute(FSTterminal.getType(), FSTterminal.getName());

			} else {
				FSTNonTerminal FSTliteral = (FSTNonTerminal)FSTnode;
				Element literal = xmi.createElement("UML:EnumerationLiteral");
				literal.setAttribute("name", FSTliteral.getName());
				for(FSTNode FSTsubliteral : FSTliteral.getChildren()) {
					FSTTerminal FSTsub = (FSTTerminal)FSTsubliteral;
					literal.setAttribute(FSTsub.getType(), FSTsub.getName());
				}
				enumliteral.appendChild(literal);
			}
		}
		return enumeration;
	}
	
	private Element createGeneralization(FSTNonTerminal FSTchild) {
		Element general = xmi.createElement("UML:Generalization");
		Element generalChild = xmi.createElement("UML:Generalization.child");
		Element generalParent = xmi.createElement("UML:Generalization.parent");
		Element xmiClass1 = xmi.createElement("UML:Class");
		Element xmiClass2 = xmi.createElement("UML:Class");
		linkManager.addClassLink(xmiClass1);
		linkManager.addClassLink(xmiClass2);
		//classLinks.add(xmiClass1);
		//classLinks.add(xmiClass2);
		generalChild.appendChild(xmiClass1);
		generalParent.appendChild(xmiClass2);
		general.appendChild(generalChild);
		general.appendChild(generalParent);
		for(FSTNode FSTnode : FSTchild.getChildren()) {
			FSTTerminal terminal = (FSTTerminal)FSTnode;
			String name = terminal.getName();
			String type = terminal.getType();
			if(type.equals("isSpecification")) {
				general.setAttribute("isSpecification", name);
			} else if(type.equals("Child")) {
				xmiClass1.setAttribute("xmi.idref", name);
			} else if(type.equals("Parent")) {
				xmiClass2.setAttribute("xmi.idref", name);
			}
		}
		return general;
	}

	private Element createAttribute(FSTNonTerminal FSTchild) {
		Element attributeNode = xmi.createElement("UML:Attribute");
		FSTNonTerminal attributes = (FSTNonTerminal) FSTchild;
		attributeNode.setAttribute("name", FSTchild.getName());
		String upperMult = "";
		String lowerMult = "";
		// get Attribute-Attributes
		for (FSTNode attribute : attributes.getChildren()) {

			// DataType contained?
			if (attribute.getType().equals("XMIDataType")) {
				FSTNonTerminal FSTdataType = (FSTNonTerminal) attribute;
				String idRef = linkManager.addDataType(new DataType(FSTdataType, createUniqueID()));
				// Link Attribute-Data-Type to Data-Type
				Element UMLstructFeature = xmi.createElement("UML:StructuralFeature.type");
				Element UMLdataType = xmi.createElement("UML:DataType");
				UMLdataType.setAttribute("xmi.idref", idRef);
				UMLstructFeature.appendChild(UMLdataType);
				attributeNode.appendChild(UMLstructFeature);
			//Link to Enumeration	
			} else if (attribute.getType().equals("XMIEnumeration")) {
				Element UMLstructFeature = xmi.createElement("UML:StructuralFeature.type");
				Element UMLenum = xmi.createElement("UML:Enumeration");
				UMLenum.setAttribute("xmi.idref", attribute.getName());
				UMLstructFeature.appendChild(UMLenum);
				attributeNode.appendChild(UMLstructFeature);
				//enumLinks.add(UMLenum);
				linkManager.addEnumLink(UMLenum);
			} else if (attribute.getType().equals("lower")) {
				lowerMult = attribute.getName();
			} else if (attribute.getType().equals("upper")) {
				upperMult = attribute.getName();
			} else {
				attributeNode.setAttribute(attribute.getType(), attribute
						.getName());
			}
			
			
			//attributeNode.setAttribute("xmi.id", createUniqueID());
		}
		Element structMult = xmi.createElement("UML:StructuralFeature.multiplicity");
		Element mult = xmi.createElement("UML:Multiplicity");
		Element multRange = xmi.createElement("UML:Multiplicity.range");
		Element multItemRange = xmi.createElement("UML:MultiplicityRange");
		
		multItemRange.setAttribute("upper", upperMult);
		multItemRange.setAttribute("lower", lowerMult);
		multRange.appendChild(multItemRange);
		mult.appendChild(multRange);
		structMult.appendChild(mult);
		attributeNode.appendChild(structMult);
		return attributeNode;
	}

	private Element createOperation(FSTNonTerminal FSTchild) {
		Element operationNode = xmi.createElement("UML:Operation");
		operationNode.setAttribute("name", FSTchild.getName());
		FSTNonTerminal operations = (FSTNonTerminal) FSTchild;
		Element parameterSubType = xmi.createElement("Dummy");
		// get Operation-Attributes
		for (FSTNode operation : operations.getChildren()) {
			if (operation.getType().equals("XMIOperationParam")) {
				Element behavioralFeature = xmi
						.createElement("UML:BehavioralFeature.parameter");
				Element parameter = xmi.createElement("UML:Parameter");
				Element parameterType = xmi.createElement("UML:Parameter.type");
				

				FSTNonTerminal FSTNToperation = (FSTNonTerminal) operation;
				for (FSTNode FSTparameter : FSTNToperation.getChildren()) {
					String name = FSTparameter.getType();
					if (name.equals("DataType")) {
						parameterSubType.setNodeValue("UML:DataType");
					} else if (name.equals("Class")) {
						parameterSubType = xmi.createElement("UML:Class");
						parameterSubType.setAttribute("xmi.idref", FSTparameter
								.getName());
						linkManager.addClassLink(parameterSubType);
					} else {
						parameter.setAttribute(name, FSTparameter.getName());
					}
				}

				parameterType.appendChild(parameterSubType);
				parameter.appendChild(parameterType);
				behavioralFeature.appendChild(parameter);
				operationNode.appendChild(behavioralFeature);
			} else if (operation.getType().equals("XMIDataType")) {
				FSTNonTerminal node = (FSTNonTerminal) operation;
				String idRef = linkManager.addDataType(new DataType(node,createUniqueID()));
				parameterSubType.setAttribute("xmi.idref", idRef);
			} else {
				operationNode.setAttribute(operation.getType(), operation
						.getName());
			}
			//operationNode.setAttribute("xmi.id", createUniqueID());
		}
		return operationNode;
	}

	private Element createRoot() {
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();

		try {
			DocumentBuilder db = dbf.newDocumentBuilder();
			xmi = db.newDocument();
		} catch (ParserConfigurationException pce) {
			pce.printStackTrace();
		}

		// XMI
		Element root = xmi.createElement("XMI");
		root.setAttribute("xmi.version", "1.2");
		root.setAttribute("xmlns:UML", "org.omg.xmi.namespace.UML");
		// Header
		Element header = xmi.createElement("XMI.header");
		Element metamodel = xmi.createElement("XMI.metamodel");
		metamodel.setAttribute("xmi.name", "UML");
		metamodel.setAttribute("xmi.version", "1.4");
		header.appendChild(metamodel);
		// Content
		Element content = xmi.createElement("XMI.content");
		// Model
		Element model = xmi.createElement("UML:Model");
		model.setAttribute("xmi.id", "");
		// Namespace
		Element namespace = xmi.createElement("UML:Namespace.ownedElement");
		model.appendChild(namespace);
		content.appendChild(model);
		root.appendChild(content);
		root.appendChild(header);
		xmi.appendChild(root);

		return namespace;
	}

	private String createUniqueID() {
		uniqueID++;
		return uniqueID.toString();
	}

	/*
	private void createClassLinks() {
		for (Element classLink : classLinks) {
			String idRef = classLink.getAttribute("xmi.idref");
			classLink.setAttribute("xmi.idref", refMap.get(idRef));
		}
	
	
	private void createEnumLinks() {
		for (Element enumLink : enumLinks) {
			String idRef = enumLink.getAttribute("xmi.idref");
			String newID = enumMap.get(idRef); 
			enumLink.setAttribute("xmi.idref", newID);
		}
	}*/


}
