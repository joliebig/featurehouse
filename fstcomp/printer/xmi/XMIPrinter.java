package printer.xmi;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;

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

import builder.xmi.XMIAssociation;
import builder.xmi.XMIAssociationClass;
import builder.xmi.XMIClass;
import builder.xmi.XMIDataType;
import builder.xmi.XMIEnumeration;
import builder.xmi.XMIGeneralization;
import builder.xmi.XMINode;
import builder.xmi.XMIPackage;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class XMIPrinter {

	FSTNode root;
	String filename;
	Document xmi = null;
	Element documentRoot;


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
		for (FSTNode node : nonterminal.getChildren()) {
			
			XMINode xminode = (XMINode) node;
			documentRoot.appendChild(xminode.toXMI(xmi));
			
			System.out.println(node.getClass());
			
			/*if (node instanceof XMIClass) {
				XMIClass xmiClass = (XMIClass)node;
				Element classNode = xmiClass.toXMI(xmi);
				documentRoot.appendChild(classNode);
			} else if (node instanceof XMIDataType) {
				documentRoot.appendChild(((XMIDataType)node).toXMI(xmi));
			} else if (node instanceof XMIAssociation) {
				documentRoot.appendChild(((XMIAssociation)node).toXMI(xmi));
			} else if (node instanceof XMIEnumeration) {
				documentRoot.appendChild(((XMIEnumeration) node).toXMI(xmi));
			} else if (node instanceof XMIGeneralization) {
				documentRoot.appendChild(((XMIGeneralization) node).toXMI(xmi));
			} else if (node instanceof XMIAssociationClass) {
				documentRoot.appendChild(((XMIAssociationClass) node).toXMI(xmi));
			} else if (node instanceof XMIPackage) {
				documentRoot.appendChild(((XMIPackage) node).toXMI(xmi));
			}*/
		}
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

}
