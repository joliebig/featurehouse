package builder.xml;

import de.ovgu.cide.fstgen.ast.FSTTerminal;
public class XMLAttribute extends FSTTerminal{

	public XMLAttribute(String type, String name, String body, String prefix) {
		super(type, name, body, prefix);
	}
	
	public XMLAttribute(String type, String name, String body) {
		super(type, name, body, "");
	}


}
