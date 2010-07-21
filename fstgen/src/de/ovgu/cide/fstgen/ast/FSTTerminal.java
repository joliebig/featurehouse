package de.ovgu.cide.fstgen.ast;

public class FSTTerminal extends FSTNode {

	public final static String defaultCompositionMechanism = "Replacement";
	public final static String defaultMergingMechanism = "Default";

	private String body;

	private String compose = defaultCompositionMechanism;
	private String merge = defaultMergingMechanism;

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

	public FSTTerminal(String type, String name, String body, String prefix,
			String compositionMechanism, String mergingMechanism) {
		this(type, name, body, prefix, compositionMechanism);
		this.merge = mergingMechanism;
	}

	public String getSpecialTokenPrefix() {
		return prefix;
	}

	@Override
	public FSTNode getShallowClone() {
		return new FSTTerminal(getType(), getName(), getBody(),
				getSpecialTokenPrefix(), getCompositionMechanism(),
				getMergingMechanism());
	}

	@Override
	public FSTNode getDeepClone() {
		return new FSTTerminal(getType(), getName(), getBody(),
				getSpecialTokenPrefix(), getCompositionMechanism(),
				getMergingMechanism());
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

	public String getMergingMechanism() {
		return merge;
	}

	@Override
	public String toString() {
		return "[T -> " + getName() + " : " + getType()
				+ " \""
				// + (prefix.length() != 0 ? prefix.replaceAll("\\s", " ") : "")
				// + "\" \""
				+ (body.length() != 0 ? body.replaceAll("\\s", " ") : "")
				+ "\" compose:" + compose + " merge: " + merge + "]";
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

	public void setCompositionMechanism(String compositionMechanism) {
		compose = compositionMechanism;
	}

	public void setMergingMechanism(String mergingMechanism) {
		merge = mergingMechanism;
	}

}
