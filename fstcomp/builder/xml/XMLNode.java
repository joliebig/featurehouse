package builder.xml;

import java.util.HashMap;
import java.util.Map;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class XMLNode extends FSTNonTerminal {

	/**
	 * XML node
	 */
	private Node node;

	/**
	 * The root node
	 */
	private Node root;


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
	
	private XMLHook hook = null;

	/**
	 * Creates a new XMLNode
	 * @param node the XML node
	 * @param root the XML root node
	 * @param ignoreID <tt>true</tt>, to enable ignore mode
	 * @param copyMode <tt>true</tt>, to enable copyMode
	 */
	public XMLNode(Node node, Node root, boolean ignoreID, boolean copyMode) {
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
			int len = map.getLength();
			
			for (int i = 0; i < len; i++) {
				Node subNode = map.item(i);

				String nodeName = subNode.getNodeName();
				String nodeValue = subNode.getNodeValue();

				if (nodeName.equals("android:id")) {
					// setNodeAttribute("xmi.id",
					// nodeValue);//node.getNodeName() + xmiName);
					// setName(nodeValue);
					id = nodeValue;
				} else if (nodeName.equals("xmi.idref")) {
					// setNodeAttribute("xmi.idref", node.getNodeName() +
					// IdToElement(nodeValue, node.getNodeName()));
					ref = nodeValue;
				} else if (nodeName.equals("android:name")) {
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
					setNodeAttribute("android:id", id);
					setName(id);
				}

				if (!name.isEmpty()) {
					setNodeAttribute("android:name", name);
				}

				if (!ref.isEmpty()) {
					setNodeAttribute("android:idref", type + IdToElement(ref, type));
				}

			} else {

			if (name.equals("")) {
				if (!id.equals("")) {
					if (!ignoreID) {
						setNodeAttribute("android:id", id);
						setName(id);
					}

				}
			} else {
				setName(name);
				setNodeAttribute("name", name);
				if (!id.equals("")) {
					setNodeAttribute("android:id",  type + name);
				}
			}

			if (!ref.equals("")) {
				String newRef = IdToElement(ref, type);
				if (newRef.equals("")) {
					setNodeAttribute("android:idref", ref);
				} else {
					setNodeAttribute("android:idref", type + newRef);
				}
			}

			}

		}

	}

	/**
	 * Makes a FST out of a XML document
	 */
	public void toFST() {
		NodeList children = node.getChildNodes();
		int len = children.getLength();
		for (int i = 0; i < len; i++) {
			Node child = children.item(i);
			//System.out.println(child.getNodeName() + " " + child.getTextContent());
			if (child.getNodeName().equals("#comment")){
				extractComment(child);
			} else if (!child.getNodeName().equals("#text")) {
				XMLNode xmlNode = new XMLNode(child,
						getRoot(), ignoreID, copyMode);
				xmlNode.setNodeAttribute("#text", getFirstLevelTextContent(child));
				xmlNode.toFST();
				if (hook==null){
					addChild(xmlNode);
				} else {
					hook.addChild(xmlNode);
				}
			}
		}

	}
	
	private void extractComment(Node node) {
		String commentText = node.getTextContent();
		//System.out.println( commentText );
		if (commentText.matches("(?s).*\\s*@start\\s*.*")) {
			hook = new XMLHook(node, root, ignoreID, copyMode, commentText);
			addChild(hook);
		} else if (commentText.matches("(?s).*\\s*@end\\s*.*")) {
			hook = null;
		}
		
	}
	
	/**
	 * Gets the text content from the current node only, leaving text content
	 * of all descendants
	 * @param node The node to extract text content
	 * @return The text content from that node only
	 */
	public static String getFirstLevelTextContent(Node node) {
	    NodeList list = node.getChildNodes();
	    StringBuilder textContent = new StringBuilder();
	    int len = list.getLength();
	    for (int i = 0; i < len; ++i) {
	        Node child = list.item(i);
	        if (child.getNodeType() == Node.TEXT_NODE)
	            textContent.append(child.getTextContent());
	    }
	    return textContent.toString();
	}

	/**
	 * Makes a XMI document out of FST
	 * @param doc the root of a XMI document
	 * @return the root of the XMI tree
	 */
	public Element toXML(Document doc) {

		Element node = doc.createElement(getType());

		Map<String, String> attributes = getAttributes();
		
		String text = attributes.remove("#text");
		node.setTextContent(text);
		
	/*	for (String key : attributes.keySet()) {
			node.setAttribute(key, attributes.get(key));
		} */

		for (FSTNode fstnode : getChildren()) {
			if (fstnode instanceof XMLNode) {
				XMLNode xmlnode = (XMLNode) fstnode;
				if (xmlnode.getType().equals("#comment")) {
					doc.createComment(xmlnode.getName());
				} else {
					node.appendChild(xmlnode.toXML(doc));
				}
			} else {
				FSTTerminal xmlAttr = (FSTTerminal) fstnode;
				if (xmlAttr.getType().equals("attribute")) {
					//System.out.println(xmlAttr.getName() + " + " + xmlAttr.getBody());
					node.setAttribute(xmlAttr.getName(), xmlAttr.getBody());
				}
			}
			
		}

		return node;
	}

	@Override
	public FSTNode getShallowClone() {
		return new XMLNode(node, getRoot(), ignoreID, copyMode);
	}

	@Override
	public FSTNode getDeepClone() {
		XMLNode clone = new XMLNode(node.cloneNode(true), getRoot(), ignoreID, copyMode);

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
		if (! name.equalsIgnoreCase("#text")){
			XMLAttribute attr = new XMLAttribute("attribute",name, value);
			addChild(attr);
		}
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
	public Node getRoot() {
		return root;
	}

	/**
	 * Replaces the id through through the name the id refers to
	 * @param id the id to resolve
	 * @param rootType the node type the id belongs to
	 * @return a non ID representation
	 */
	public String IdToElement(String id, String rootType) {
		NodeList dataTypes = root.getChildNodes();
		int len = dataTypes.getLength();
		for (int i = 0; i < len; i++) {
			Element dataType = (Element) dataTypes.item(i);
			//System.out.println(dataType.getParentNode().toString());
			if (dataType.getAttribute("android:id").equals(id)) {
				return dataType.getAttribute("name");
			}
		}
		System.err.println("Can't find " + id + " in " + rootType);
		return id;
	}


}
