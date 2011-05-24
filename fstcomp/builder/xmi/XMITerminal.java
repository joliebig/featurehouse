package builder.xmi;

import java.util.HashMap;
import java.util.Map;

import de.ovgu.cide.fstgen.ast.FSTTerminal;

public abstract class XMITerminal extends FSTTerminal implements XMINodeInterface {
	
	private Map<String, String> nodeAttributes = new HashMap<String, String>();

	public XMITerminal(String type, String name) {
		super(type, name, "", "");
	}
	
	public void setNodeAttribute(String name, String value) {
		nodeAttributes.put(name, value);
	}
	
	public String getNodeAttribute(String name) {
		return nodeAttributes.get(name);
	}
}
