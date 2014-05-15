package printer.xml;

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

import builder.xml.XMLNode;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class XMLPrinter {

	FSTNode root;
	String filename;
	Document xml = null;

	public XMLPrinter(FSTNode root, String filename) {
		this.root = root;
		this.filename = filename;
	}

	/**
	 * Prints the result into an new XML-file
	 * 
	 * @param filename
	 */
	public void transformDocument() {
		Transformer transformer;

		createRoot();
		process();
		try {
			transformer = TransformerFactory.newInstance().newTransformer();
			DOMSource source = new DOMSource(xml);
			FileOutputStream os = new FileOutputStream(filename);
			StreamResult result = new StreamResult(os);
			transformer.transform(source, result);
		} catch (TransformerConfigurationException e) {
			e.printStackTrace();
		} catch (TransformerFactoryConfigurationError e) {
			e.printStackTrace();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (TransformerException e) {
			e.printStackTrace();
		}
	}

	private void process() {
		FSTNonTerminal nonterminal = (FSTNonTerminal) root;
		for (FSTNode node : nonterminal.getChildren()) {
			XMLNode xmlnode = (XMLNode) node;
			if (xmlnode.getType().equals("#comment")) {
				xml.createComment(xmlnode.getName());
			} else {
				xml.appendChild(xmlnode.toXML(xml));
			}
		}
	}

	private void createRoot() {
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();

		try {
			DocumentBuilder db = dbf.newDocumentBuilder();
			xml = db.newDocument();
		} catch (ParserConfigurationException pce) {
			pce.printStackTrace();
		}
	}

}
