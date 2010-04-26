package builder.xmi;

import java.util.HashMap;
import java.util.Map;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class XMINode extends FSTNonTerminal {

	/**
	 * XML node
	 */
	private Node node;
	
	/**
	 * The root node
	 */
	private Element root;
	
	
	/**
	 * A map containing all node attributes.
	 */
	private Map<String, String> nodeAttributes = new HashMap<String, String>();
	
	/**
	 * When this flag is set, the ID of a node won't be used for comparing.
	 */
	private boolean ignoreID;
	
	/**
	 * When this flag is set, the id of a node won't be replaced through a new
	 * identifier. The existing ID will be used.
	 */
	private boolean copyMode;

	/**
	 * Creates a new XMINode
	 * @param node the XMI node
	 * @param root the XMI root node
	 * @param ignoreID <tt>true</tt>, to enable ignore mode
	 * @param copyMode <tt>true</tt>, to enable copyMode
	 */
	public XMINode(Node node, Element root, boolean ignoreID, boolean copyMode) {
		super(node.getNodeName(), "");
		//super(node.getNodeName(), "", root);
		//this.type = node.getNodeName();
		this.root = root;
		this.node = node;
		this.ignoreID = ignoreID;
		this.copyMode = copyMode;

		String name = "";
		String type = "";
		String id = "";
		String ref = "";
		

		type = node.getNodeName();

		NamedNodeMap map = node.getAttributes();
		if (map != null) {
			for (int i = 0; i < map.getLength(); i++) {
				Node subNode = map.item(i);

				String nodeName = subNode.getNodeName();
				String nodeValue = subNode.getNodeValue();

				if (nodeName.equals("xmi.id")) {
					// setNodeAttribute("xmi.id",
					// nodeValue);//node.getNodeName() + xmiName);
					// setName(nodeValue);
					id = nodeValue;
				} else if (nodeName.equals("xmi.idref")) {
					// setNodeAttribute("xmi.idref", node.getNodeName() +
					// IdToElement(nodeValue, node.getNodeName()));
					ref = nodeValue;
				} else if (nodeName.equals("name")) {
					// setNodeAttribute("name", nodeValue);
					name = nodeValue;
				} else {
					setNodeAttribute(nodeName, nodeValue);
				}
			}
			
			
			

			/*if (ignoreID && (!id.equals("")) && (!name.equals(""))) {
				
				System.out.println(type + " " + ignoreID);
			} else*/ 
			
			if (copyMode) {
				if (!id.isEmpty()) {
					setNodeAttribute("xmi.id", id);
					setName(id);
				}
				
				if (!name.isEmpty()) {
					setNodeAttribute("name", name);
				}
				
				if (!ref.isEmpty()) {
					setNodeAttribute("xmi.idref", type + IdToElement(ref, type));
				}
				
			} else {
			
			if (name.equals("")) {
				if (!id.equals("")) {
					if (!ignoreID) {
						setNodeAttribute("xmi.id", id);
						setName(id);
					}
					
				}
			} else {
				setName(name);
				setNodeAttribute("name", name);
				if (type.equals("UML:Parameter") | type.equals("UML:Attribute") | type.equals("UML:Operation")) {
					//setNodeAttribute("xmi.id",  id);
				} else if (ignoreID) {
					
				} else if (!id.equals("")) {
					setNodeAttribute("xmi.id",  type + name);
				} 
			}

			if (!ref.equals("")) {
				String newRef = IdToElement(ref, type);
				if (newRef.equals("")) {
					setNodeAttribute("xmi.idref", ref);
				} else {
					setNodeAttribute("xmi.idref", type + newRef);
				}
			}
			
			}
			
			if (type.equals("UML:Multiplicity") || type.equals("UML:MultiplicityRange")) {
				setName("id");
			}
			
			if (type.equals("UML:Association.connection") || type.equals("UML:ClassifierRole.multiplicity")) {
				this.ignoreID = true;	
			}
			
			if (type.equals("UML:Collaboration.interaction")) {
				this.copyMode = true;
			}
			
		}

	}

	/**
	 * Makes a FST out of a XMI document
	 */
	public void toFST() {
		NodeList children = node.getChildNodes();

		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if (!child.getNodeName().equals("#text")) {
				XMINode xmiNode = new XMINode(child,
						getRoot(), ignoreID, copyMode);
				xmiNode.toFST();
				addChild(xmiNode);
			}
		}

	}

	/**
	 * Makes a XMI document out of FST
	 * @param doc the root of a XMI document
	 * @return the root of the XMI tree
	 */
	public Element toXMI(Document doc) {

		Element node = doc.createElement(getType());

		Map<String, String> attributes = getAttributes();

		for (String key : attributes.keySet()) {
			node.setAttribute(key, attributes.get(key));
		}

		for (FSTNode fstnode : getChildren()) {
			XMINode xminode = (XMINode) fstnode;
			node.appendChild(xminode.toXMI(doc));
		}

		return node;
	}

	@Override
	public FSTNode getShallowClone() {
		return new XMINode(node, getRoot(), ignoreID, copyMode);
	}

	@Override
	public FSTNode getDeepClone() {
		XMINode clone = new XMINode(node.cloneNode(true), getRoot(), ignoreID, copyMode);

		for (FSTNode fstNode : getChildren()) {
			clone.addChild(fstNode.getDeepClone());
		}

		return clone;
	}
	
	/**
	 * Sets an attribute of this node.
	 * @param name the attribute identifier
	 * @param value the attribute value
	 */
	public void setNodeAttribute(String name, String value) {
		nodeAttributes.put(name, value);
	}
	
	/**
	 * @return the whole node attribute map.
	 */
	public Map<String, String> getAttributes() {
		return nodeAttributes;
	}
	
	/**
	 * Return the value of the specified attribute.
	 * @param name the name of the attribute
	 * @return the value of the attribute
	 */
	public String getNodeAttribute(String name) {
		return nodeAttributes.get(name);
	}
	
	/**
	 * @return the current node
	 */
	public Element getNode() {
		return (Element) node;
	}
	
	/**
	 * @return the root node
	 */
	public Element getRoot() {
		return root;
	}
	
	/**
	 * Replaces the id through through the name the id refers to
	 * @param id the id to resolve
	 * @param rootType the node type the id belongs to
	 * @return a non ID representation
	 */
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
