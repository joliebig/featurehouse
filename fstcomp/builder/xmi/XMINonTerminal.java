package builder.xmi;

import java.util.HashMap;
import java.util.Map;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public abstract class XMINonTerminal extends FSTNonTerminal implements XMINodeInterface {
	
	private Map<String, String> nodeAttributes = new HashMap<String, String>();
	private Element root;
	private Node node;

	public XMINonTerminal(String type, String name, Node node, Element root) {
		super(type, name);
		this.root = root;
		this.node = node;
	}
	
	public XMINonTerminal(String type, String name, Element root) {
		super(type, name);
		this.root = root;
		//this.node = node;
	}
	
	
	
	public void setNodeAttribute(String name, String value) {
		nodeAttributes.put(name, value);
	}
	
	public Map<String, String> getAttributes() {
		return nodeAttributes;
	}
	
	public String getNodeAttribute(String name) {
		return nodeAttributes.get(name);
	}
	
	public Element getNode() {
		return (Element) node;
	}
	
	public Element getRoot() {
		return root;
	}
	
	
	public String IdToElement(String id, String rootType) {
		NodeList dataTypes = root.getElementsByTagName(rootType);
		for (int i = 0; i < dataTypes.getLength(); i++) {
			Element dataType = (Element) dataTypes.item(i);
			//System.out.println(dataType.getParentNode().toString());
			if (dataType.getAttribute("xmi.id").equals(id)) {
				return dataType.getAttribute("name");
			}
		}
		System.err.println("Can't find " + id + " in " + rootType);
		return id;
	}
}
