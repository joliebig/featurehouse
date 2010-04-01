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

import builder.xmi.XMINode;
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

		//System.out.println(root.toString());

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
		// header
		Element header = xmi.createElement("XMI.header");
		Element metamodel = xmi.createElement("XMI.metamodel");
		metamodel.setAttribute("xmi.name", "UML");
		metamodel.setAttribute("xmi.version", "1.4");
		header.appendChild(metamodel);
		// content
		Element content = xmi.createElement("XMI.content");
		// model
		Element model = xmi.createElement("UML:Model");
		model.setAttribute("xmi.id", "");
		// namespace
		Element namespace = xmi.createElement("UML:Namespace.ownedElement");
		model.appendChild(namespace);
		content.appendChild(model);
		root.appendChild(content);
		root.appendChild(header);
		xmi.appendChild(root);

		return namespace;
	}

}
