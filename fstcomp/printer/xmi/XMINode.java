package printer.xmi;

import java.util.HashMap;
import java.util.Map;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class XMINode {
	private Map<String, String> attributeMap = new HashMap<String,String>();
	
	public XMINode() {}

	public XMINode(FSTNonTerminal node, String id) {
		setAttribute(Strings.ID, id);
		setAttribute(Strings.NAME, node.getName());
		for (FSTNode dataTypeAttribute : node.getChildren()) {
			String type = dataTypeAttribute.getType();
			String nodeName = dataTypeAttribute.getName();
			
			if (type.equals("name") && getAttribute(Strings.NAME).equals("")) {
				setAttribute(Strings.NAME, node.getName());
			}
			
			setAttribute(type, nodeName);
		}
	}
	
	public void setAttribute(String name, String value) {
		attributeMap.put(name, value);
	}
	
	public String getAttribute(String name) {
		return attributeMap.get(name);
	}
	
	@Override
	public boolean equals(Object o) {
		if (o == null) {
			return false;
		} else {
			if (o instanceof DataType) {
				DataType ref = (DataType)o;
				if (ref.getAttribute(Strings.NAME).equals(getAttribute(Strings.NAME)) &&
					ref.getAttribute(Strings.ISABSTRACT).equals(getAttribute(Strings.ISABSTRACT)) &&
					ref.getAttribute(Strings.ISLEAF).equals(getAttribute(Strings.ISLEAF)) &&
					ref.getAttribute(Strings.ISROOT).equals(getAttribute(Strings.ISROOT)) &&
					ref.getAttribute(Strings.ISSPEC).equals(getAttribute(Strings.ISSPEC))) {
					return true;
				}
			}
			return false;
		}
	}
	
}
