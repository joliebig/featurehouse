package builder.xmi;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import de.ovgu.cide.fstgen.ast.FSTNode;

public class XMIPackage extends XMINonTerminal {

	public XMIPackage(Element node, Element root) {
		super("XMIPackage", node.getAttribute("name"), node, root);
		
		NodeList packageNodes = node.getElementsByTagName("UML:Package");
		for (int j = 0; j < packageNodes.getLength(); j++) {
			Element subPackage = (Element) packageNodes.item(j);
			extractPackage(subPackage);
		}
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
			//Folgezeile evtl wieder einkommentieren...
			//FSTroot.addChild(extractClass(XMIclass));
			
			//DUMMY!!!
			XMIclass.setAttribute("visibility", "false");
			
			addChild(new XMIClass((Element) XMIclass, getRoot()));
			
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
	
	@Override
	public Element toXMI(Document doc) {
		for (FSTNode node : getChildren())  {
			XMIClass xmiclass = (XMIClass) node;
			return xmiclass.toXMI(doc);
		}
		return null;
	}
	
	@Override
	public FSTNode getShallowClone() {
		return new XMIPackage(getNode(), getRoot());
	}

	@Override
	public FSTNode getDeepClone() {
		return new XMIPackage(getNode(), getRoot());
	}
}
