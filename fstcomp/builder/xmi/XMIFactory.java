package builder.xmi;

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

public class XMIFactory {

	// XMI-document
	private Document xmi;
	// Content-root in XMI-document
	// private Element root;
	// Root in FST
	private FSTNonTerminal FSTMasterRoot;

	Element docEle;

	public XMIFactory(File filename, FSTNonTerminal fstroot) throws ParseException{
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();

		try {
			DocumentBuilder db = dbf.newDocumentBuilder();
			xmi = db.parse(filename);
		} catch (ParserConfigurationException pce) {
			pce.printStackTrace();
		} catch (SAXException se) {
//			se.printStackTrace();
			throw new ParseException(se.toString());
		} catch (IOException ioe) {
			ioe.printStackTrace();
		}

		FSTMasterRoot = fstroot;
		docEle = xmi.getDocumentElement();

	}

	public void extract() {
		extractFST(docEle, FSTMasterRoot);
	}

	/**
	 * Makes an FST out of the XMI-file
	 */
	private void extractFST(Element masterRoot, FSTNonTerminal FSTroot) {
		
		AbstractFSTParser.fstnodes.add(FSTroot);

		NodeList modelList = masterRoot
				.getElementsByTagName("UML:Namespace.ownedElement");
		if (modelList.getLength() > 0) {
			Element root = (Element) modelList.item(0);

			NodeList childNodes = root.getChildNodes();

			for (int i = 0; i < childNodes.getLength(); i++) {
				Node node = childNodes.item(i);

				String nodeName = node.getNodeName();

				 if (!nodeName.equals("#text")) {
					XMINode xmiab = new XMINode(node, root, false, false);
					xmiab.toFST();
					FSTroot.addChild(xmiab);
				}
			}
		}


	}


}
