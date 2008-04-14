package de.ovgu.cide.fstgen.ast;

public class FSTTerminal extends FSTNode {

	private String body;
	private String compose = "error";

	public FSTTerminal(String type, String name, String body) {
		super(type, name);
		this.body = body;
	}

	public FSTTerminal(String type, String name, String body,
			String compositionMechanism) {
		this(type, name, body);
		this.compose = compositionMechanism;
	}

	public String getBody() {
		return body;
	}

	public String getCompositionMechanism() {
		return compose;
	}

	@Override
	public String toString() {
		return "[T: " + getType() + "/" + getName() + " \""
				+ body.replaceAll("\\s", " ") + "\" compose:"+compose+"]";
	}

	public String printFST(int indent) {
		StringBuffer buffer = new StringBuffer();
		for (int i = 0; i < indent; i++)
			buffer.append("    ");
		buffer.append(this.toString());
		buffer.append("\n");
		return buffer.toString();
	}
	
	@Override
	public void accept(FSTVisitor visitor) {
		visitor.visit(this);
		visitor.postVisit(this);
	}

}
