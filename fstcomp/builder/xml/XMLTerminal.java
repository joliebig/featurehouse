package builder.xml;

import java.util.HashMap;
import java.util.Map;

import de.ovgu.cide.fstgen.ast.FSTTerminal;

public abstract class XMLTerminal extends FSTTerminal implements XMLNodeInterface {
	
	private Map<String, String> nodeAttributes = new HashMap<String, String>();

	public XMLTerminal(String type, String name, String body) {
		super(type, name, body, "");
	}
	
	public void setNodeAttribute(String name, String value) {
		nodeAttributes.put(name, value);
	}
	
	public String getNodeAttribute(String name) {
		return nodeAttributes.get(name);
	}
}
