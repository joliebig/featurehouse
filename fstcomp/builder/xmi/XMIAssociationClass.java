package builder.xmi;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;


import de.ovgu.cide.fstgen.ast.FSTNode;

public class XMIAssociationClass extends XMIClass {
	
	//XMIAssociation xmiassociation;
	
	public XMIAssociationClass(Element node, Element root)  {
		super("XMIClass", node, root);
		addChild(new XMIAssociation(node, root));
	}
	
	public void toFST() {
		super.toFST();
		for(FSTNode node : getChildren()) {
			if (node instanceof XMIAssociation) {
				((XMIAssociation) node).toFST();
			}
		}
	}
	
	@Override
	public Element toXMI(Document doc) {
		Element node = super.toXMI(doc);
		Element root = doc.createElement("UML:AssociationClass");
		NodeList classifiers = node.getChildNodes();
		Element classifier = (Element) classifiers.item(0);
		for(XMIClassAttributes attribute : XMIClassAttributes.values()) {
			root.setAttribute(attribute.toString(), getNodeAttribute(attribute.toString()));
		}
		root.setAttribute("xmi.id", "class"  + getNodeAttribute("name"));
		root.appendChild(classifier);
		
		for(FSTNode assoc : getChildren()) {
			if (assoc instanceof XMIAssociation) {
				Element assocnode = ((XMIAssociation) assoc).toXMI(doc);
				NodeList assocchildren = assocnode.getChildNodes();
				Element assochchild = (Element) assocchildren.item(0);
				root.appendChild(assochchild);
			}
		}
		
		root.appendChild(classifier);
		
		return root;
	}
	
	@Override
	public FSTNode getShallowClone() {
		return new XMIAssociationClass(getNode(), getRoot());
	}

	@Override
	public FSTNode getDeepClone() {
		XMIClass clone = new XMIAssociationClass(getNode(), getRoot());
		clone.toFST();
		return clone;
	}

}
