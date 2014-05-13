package builder.xml;

import org.w3c.dom.Node;

public class XMLHook extends XMLNode {

	public XMLHook(Node node, Node root, boolean ignoreID, boolean copyMode, String commentText) {
		super(node, root, ignoreID, copyMode);
		String attribute = commentText.replace("@start ", "").trim();
		
		if ( attribute.matches("(?s).*\\s*before\\s*.*")) {
			this.setType("before");
			attribute = attribute.replace("before", "").trim();
		} else if (attribute.matches("(?s).*\\s*after\\s*.*")) {
			this.setType("after");
			attribute = attribute.replace("after", "").trim();
		}
		
        attribute = attribute.replace("android:id=", "");
        attribute = attribute.replaceAll("\"", "");
        this.setName(attribute);
		setNodeAttribute("android:id", attribute);		
	}


}
