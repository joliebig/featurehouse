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

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class XMIFactory {

	// XMI-document
	private Document xmi;
	// Content-root in XMI-document
	private Element root;
	// Root in FST
	private FSTNonTerminal FSTroot;
	

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
		NodeList modelList = docEle.getElementsByTagName("UML:Namespace.ownedElement");

		FSTroot = fstroot;

		if (modelList.getLength() > 0) {
			root = (Element) modelList.item(0);
		}
	}

	/**
	 * Makes an FST out of the XMI-file
	 */
	public void extractFST() {

		NodeList childNodes = root.getChildNodes();

		for (int i = 0; i < childNodes.getLength(); i++) {
			Node node = childNodes.item(i);
			
			//Class-Diagram
			String nodeName = node.getNodeName();
			if (nodeName.equals("UML:Class")) {
				XMIClass xmiclass = new XMIClass((Element) node, root);
				xmiclass.toFST();
				FSTroot.addChild(xmiclass);
			} else if (nodeName.equals("UML:DataType")) {
				FSTroot.addChild(new XMIDataType((Element) node));
			} else if (nodeName.equals("UML:Association")) {
				XMIAssociation xmiassoc = new XMIAssociation((Element) node, root);
				xmiassoc.toFST();
				FSTroot.addChild(xmiassoc);
			} else if (nodeName.equals("UML:AssociationClass")) {
				XMIAssociationClass xmiassocclass = new XMIAssociationClass((Element) node, root);
				xmiassocclass.toFST();
				FSTroot.addChild(xmiassocclass);
			} else if (nodeName.equals("UML:Generalization")) {
				XMIGeneralization xmigen = new XMIGeneralization((Element) node, root);
				xmigen.toFST();
				FSTroot.addChild(xmigen);
			} else if (nodeName.equals("UML:Enumeration")) {
				XMIEnumeration xmienum = new XMIEnumeration((Element) node, root);
				xmienum.toFST();
				FSTroot.addChild(xmienum);
			} else if (nodeName.equals("UML:Package")) {
				FSTroot.addChild(new XMIPackage((Element) node, root));
			}
		}
	}
}
