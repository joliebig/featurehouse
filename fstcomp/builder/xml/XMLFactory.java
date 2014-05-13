package builder.xml;

import java.io.File;
import java.io.IOException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.AbstractFSTParser;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class XMLFactory {

	// XML-document
	private Document xml;
	// Content-root in XML-document
	// private Element root;
	// Root in FST
	private FSTNonTerminal FSTMasterRoot;

	Element docEle;

	public XMLFactory(File filename, FSTNonTerminal fstroot)
			throws ParseException {
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();

		try {
			DocumentBuilder db = dbf.newDocumentBuilder();
			xml = db.parse(filename);
		} catch (ParserConfigurationException pce) {
			pce.printStackTrace();
		} catch (SAXException se) {
			throw new ParseException(se.toString());
		} catch (IOException ioe) {
			ioe.printStackTrace();
		}

		FSTMasterRoot = fstroot;
		Element root = xml.getDocumentElement();
		docEle  = xml.createElement("document");
		docEle.appendChild(root);

	}

	public void extract() {
		extractFST(docEle, FSTMasterRoot);
	}

	/**
	 * Makes an FST out of the XML-file
	 */
	private void extractFST(Element masterRoot, FSTNonTerminal FSTroot) {

		AbstractFSTParser.fstnodes.add(FSTroot);
		
		NodeList childNodes = masterRoot.getChildNodes();

		int len = childNodes.getLength();
		for (int i = 0; i < len; i++) {
			Node node = childNodes.item(i);

			String nodeName = node.getNodeName();
			//System.out.println(node.getNodeName() + " " + node.getTextContent());
			if (!nodeName.equals("#text")) {
				XMLNode xmiab = new XMLNode(node, masterRoot, false, false);
				xmiab.toFST();
				FSTroot.addChild(xmiab);
			}
		}

	}

}
