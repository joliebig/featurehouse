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
	Map<String, String> refMap = new HashMap<String, String>();
	List<Element> classLinks = new LinkedList<Element>();

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
		createClassLinks();
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
				classNode.setAttribute("name", child.getName());

				FSTNonTerminal FSTClass = (FSTNonTerminal) child;
				for (FSTNode FSTchild : FSTClass.getChildren()) {

					if (FSTchild.getType().equals("XMIAttribute")) {
						featureNodes
								.appendChild(createAttribute((FSTNonTerminal) FSTchild));
					} else if (FSTchild.getType().equals("XMIOperation")) {
						featureNodes
								.appendChild(createOperation((FSTNonTerminal) FSTchild));
					} else {
						classNode.setAttribute(FSTchild.getType(), FSTchild
								.getName());
					}

					if (featureNodes.hasChildNodes()) {
						classNode.appendChild(featureNodes);
					}

				}
				// Save name and ID refMap for later reconstruction of
				// associations
				String IDname = classNode.getAttribute("name");
				String newID = createUniqueID();
				refMap.put(IDname, newID);

				classNode.setAttribute("xmi.id", newID);
				documentRoot.appendChild(classNode);
			} else if (child.getType().equals("XMIAssociation")) {
				documentRoot.appendChild(createAssociation(child));
			}
		}
	}

	private Element createAttribute(FSTNonTerminal FSTchild) {
		Element attributeNode = xmi.createElement("UML:Attribute");
		FSTNonTerminal attributes = (FSTNonTerminal) FSTchild;
		attributeNode.setAttribute("name", FSTchild.getName());

		// get Attribute-Attributes
		for (FSTNode attribute : attributes.getChildren()) {

			// DataType contained?
			if (attribute.getType().equals("XMIDataType")) {
				FSTNonTerminal FSTdataType = (FSTNonTerminal) attribute;
				Element dataType = xmi.createElement("UML:DataType");
				dataType.setAttribute("name", FSTdataType.getName());

				// get DataType-Attributes
				for (FSTNode dataTypeAttribute : FSTdataType.getChildren()) {
					dataType.setAttribute(dataTypeAttribute.getType(),
							dataTypeAttribute.getName());
				}

				// Link Attribute-Data-Type to Data-Type
				String dataTypeID = createUniqueID();
				dataType.setAttribute("xmi.id", dataTypeID);
				documentRoot.appendChild(dataType);

				Element UMLstructFeature = xmi
						.createElement("UML:StructuralFeature.type");
				Element UMLdataType = xmi.createElement("UML:DataType");
				UMLdataType.setAttribute("xmi.idref", dataTypeID);
				UMLstructFeature.appendChild(UMLdataType);
				attributeNode.appendChild(UMLstructFeature);
			} else {
				attributeNode.setAttribute(attribute.getType(), attribute
						.getName());
			}
			attributeNode.setAttribute("xmi.id", createUniqueID());
		}
		return attributeNode;
	}

	private Element createOperation(FSTNonTerminal FSTchild) {
		Element operationNode = xmi.createElement("UML:Operation");
		operationNode.setAttribute("name", FSTchild.getName());
		FSTNonTerminal operations = (FSTNonTerminal) FSTchild;
		String dataTypeRef = "";

		// get Operation-Attributes
		for (FSTNode operation : operations.getChildren()) {
			if (operation.getType().equals("XMIOperationParam")) {
				Element behavioralFeature = xmi
						.createElement("UML:BehavioralFeature.parameter");
				Element parameter = xmi.createElement("UML:Parameter");
				Element parameterType = xmi.createElement("UML:Parameter.type");
				Element parameterSubType = null;

				FSTNonTerminal FSTNToperation = (FSTNonTerminal) operation;
				for (FSTNode FSTparameter : FSTNToperation.getChildren()) {
					String name = FSTparameter.getType();
					if (name.equals("DataType")) {
						parameterSubType = xmi.createElement("UML:DataType");
						dataTypeRef = createUniqueID();
						parameterSubType.setAttribute("xmi.idref", dataTypeRef);

					} else if (name.equals("Class")) {
						parameterSubType = xmi.createElement("UML:Class");
						parameterSubType.setAttribute("xmi.idref", FSTparameter
								.getName());
						classLinks.add(parameterSubType);
					} else {
						parameter.setAttribute(name, FSTparameter.getName());
					}
				}

				parameterType.appendChild(parameterSubType);
				parameter.appendChild(parameterType);
				behavioralFeature.appendChild(parameter);
				operationNode.appendChild(behavioralFeature);
			} else if (operation.getType().equals("XMIDataType")) {
				Element dataType = xmi.createElement("UML:DataType");
				dataType.setAttribute("name", operation.getName());

				FSTNonTerminal FSToperation = (FSTNonTerminal) operation;
				for (FSTNode FSTdataType : FSToperation.getChildren()) {
					dataType.setAttribute(FSTdataType.getType(), FSTdataType
							.getName());
				}
				dataType.setAttribute("xmi.id", dataTypeRef);
				documentRoot.appendChild(dataType);

			} else {
				operationNode.setAttribute(operation.getType(), operation
						.getName());
			}
			operationNode.setAttribute("xmi.id", createUniqueID());
		}
		return operationNode;
	}

	private Element createAssociation(FSTNode FSTassociationNode) {
		// Association Elements
		Element association = xmi.createElement("UML:Association");
		Element associationConn = xmi
				.createElement("UML:Association.connection");

		FSTNonTerminal FSTassociation = (FSTNonTerminal) FSTassociationNode;

		association.setAttribute("name", FSTassociation.getName());

		for (FSTNode FSTassocEndNode : FSTassociation.getChildren()) {
			FSTNonTerminal FSTassocEnd = (FSTNonTerminal) FSTassocEndNode;

			if (FSTassocEnd.getType().equals("XMIAssociationDetails")) {
				for (FSTNode FSTassocSubEnd : FSTassocEnd.getChildren()) {
					FSTTerminal FSTassocTerminal = (FSTTerminal) FSTassocSubEnd;

					association.setAttribute(FSTassocTerminal.getType(),
							FSTassocTerminal.getName());

				}

			} else {

				// AssociationEnd Elements
				Element associationEnd = xmi
						.createElement("UML:AssociationEnd");
				Element associationmult = xmi
						.createElement("UML:AssociationEnd.multiplicity");
				Element multiplicity = xmi.createElement("UML:Multiplicity");
				Element multiplicityrange = xmi
						.createElement("UML:Multiplicity.range");
				Element multiplicityRange = xmi
						.createElement("UML:MultiplicityRange");
				Element participant = xmi
						.createElement("UML:AssociationEnd.participant");
				Element participantClass = xmi.createElement("UML:Class");

				// Process terminal nodes
				for (FSTNode FSTassocSubEnd : FSTassocEnd.getChildren()) {
					FSTTerminal FSTassocTerminal = (FSTTerminal) FSTassocSubEnd;

					String FSTtype = FSTassocTerminal.getType();

					if (FSTtype.equals("xmi.idref")) {
						// instead of old idref use new idref saved in the
						// refMap
						String IDname = FSTassocTerminal.getName();
						String newID = refMap.get(IDname);
						participantClass.setAttribute("xmi.idref", newID);
					} else if (FSTtype.equals("lower")
							|| FSTtype.equals("upper")) {
						multiplicityRange.setAttribute(FSTtype,
								FSTassocTerminal.getName());
					} else {
						associationEnd.setAttribute(FSTtype, FSTassocTerminal
								.getName());
					}
				}

				// Build XMI-Tree
				multiplicityrange.appendChild(multiplicityRange);
				multiplicity.appendChild(multiplicityrange);
				associationmult.appendChild(multiplicity);

				participant.appendChild(participantClass);

				associationEnd.appendChild(associationmult);
				associationEnd.appendChild(participant);

				associationConn.appendChild(associationEnd);
			}
		}

		association.appendChild(associationConn);

		return association;
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

	private void createClassLinks() {
		for (Element classLink : classLinks) {
			String idRef = classLink.getAttribute("xmi.idref");
			classLink.setAttribute("xmi.idref", refMap.get(idRef));
		}
	}

}
