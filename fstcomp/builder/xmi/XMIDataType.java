package builder.xmi;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import de.ovgu.cide.fstgen.ast.FSTNode;

public class XMIDataType extends XMITerminal {
	
	private Element root;
	
	public XMIDataType(Element root) {
		super("XMIDataType", root.getAttribute("name"));
		this.root = root;
		//Special data type attributes
		for (XMIDataTypeAttributes attribute : XMIDataTypeAttributes.values()) {
			setNodeAttribute(attribute.toString(), root.getAttribute(attribute.toString()));
		}
		setNodeAttribute("xmi.id", "dataType" + root.getAttribute("name"));
	}
	
	@Override
	public Element toXMI(Document doc) {
		Element node = doc.createElement("UML:DataType");
		//add data type node attributes
		for(XMIDataTypeAttributes attribute : XMIDataTypeAttributes.values()) {
			node.setAttribute(attribute.toString(), getNodeAttribute(attribute.toString()));
		}
		node.setAttribute("xmi.id", getNodeAttribute("xmi.id"));
		return node;
	}
	
	@Override
	public FSTNode getShallowClone() {
		//return new FSTNonTerminal(getType(), getName(), new LinkedList<FSTNode>());
		return new XMIDataType(root);
	}

	@Override
	public FSTNode getDeepClone() {
		/*LinkedList<FSTNode> cloneChildren = new LinkedList<FSTNode>();
		for(FSTNode child : getChildren())
			cloneChildren.add(child.getDeepClone());
		return new FSTNonTerminal(getType(), getName(), cloneChildren);*/
		return new XMIDataType(root);
	}
}
