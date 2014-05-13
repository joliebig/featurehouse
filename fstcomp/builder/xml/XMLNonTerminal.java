package builder.xml;

import java.util.HashMap;
import java.util.Map;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public abstract class XMLNonTerminal extends FSTNonTerminal implements XMLNodeInterface {
	
	private Map<String, String> nodeAttributes = new HashMap<String, String>();
	private Element root;
	private Node node;

	public XMLNonTerminal(String type, String name, Node node, Element root) {
		super(type, name);
		this.root = root;
		this.node = node;
	}
	
	public XMLNonTerminal(String type, String name, Element root) {
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
		
		int len = dataTypes.getLength();
		for (int i = 0; i < len; i++) {
			Element dataType = (Element) dataTypes.item(i);
			//System.out.println(dataType.getParentNode().toString());
			if (dataType.getAttribute("android.id").equals(id)) {
				return dataType.getAttribute("name");
			}
		}
		System.err.println("Can't find " + id + " in " + rootType);
		return id;
	}
}
