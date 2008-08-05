package de.ovgu.cide.fstgen.ast;


public class FSTTerminal extends FSTNode {
	
	public final static String defaultCompositionMechanism = "Replacement"; 
	
	private String body;
	private String compose = defaultCompositionMechanism;
	private String prefix;
	
	public FSTTerminal(String type, String name, String body, String prefix) {
		super(type, name);
		this.body = body;
		this.prefix = prefix;
	}

	public FSTTerminal(String type, String name, String body, String prefix,
			String compositionMechanism) {
		this(type, name, body, prefix);
		this.compose = compositionMechanism;
	}
	
	public String getSpecialTokenPrefix() {
		return prefix;
	}
	
	@Override
	public FSTNode getShallowClone() {
		return new FSTTerminal(getType(), getName(), getBody(), getSpecialTokenPrefix(), getCompositionMechanism());
	}

	@Override
	public FSTNode getDeepClone() {
		return new FSTTerminal(getType(), getName(), getBody(), getSpecialTokenPrefix(), getCompositionMechanism());
	}

	public void setBody(String b) {
		body = b;
	}

	public String getBody() {
		return body;
	}

	public String getCompositionMechanism() {
		return compose;
	}

	@Override
	public String toString() {
		return "[T -> "
				+ getName()
				+ " : "
				+ getType()
				+ " \""
				//+ (prefix.length() != 0 ? prefix.replaceAll("\\s", " ")	: "")
				//+ "\" \""
				+ (body.length() != 0 ? body.replaceAll("\\s", " ") : "")
				+ "\" compose:" + compose + "]";
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
