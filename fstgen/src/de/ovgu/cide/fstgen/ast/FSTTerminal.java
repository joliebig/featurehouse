package de.ovgu.cide.fstgen.ast;

public class FSTTerminal extends FSTNode {

	public final static String defaultCompositionMechanism = "Replacement";
	public final static String defaultMergingMechanism = "Default";

	private String body;

	private String compose = defaultCompositionMechanism;
	private String merge = defaultMergingMechanism;
	private String contractCompKey;

	private String prefix;

	public int beginLine = -1;
	public int endLine = -1;
	private String feature;

	public FSTTerminal(String type, String name, String body, String prefix) {
		super(type, name);
		this.body = body;
		this.prefix = prefix;
		if (type.equals("ContractCompKey")) {
			this.contractCompKey = body;
			this.body = "\n\t";
		}
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

	public FSTTerminal(String type, String name, String body, String prefix,
			String compositionMechanism, String mergingMechanism,
			int beginLine, int endLine) {
		this(type, name, body, prefix, compositionMechanism);
		this.merge = mergingMechanism;
		this.beginLine = beginLine;
		this.endLine = endLine;
	}

	private FSTTerminal(String type, String name, String body, String prefix,
			String compositionMechanism, String mergingMechanism, String feature) {
		this(type, name, body, prefix, compositionMechanism, mergingMechanism);
		setOriginalFeatureName(feature);
	}

	public String getSpecialTokenPrefix() {
		return prefix;
	}

	public void setSpecialTokenPrefix(String prefix) {
		this.prefix = prefix;
	}

	@Override
	public FSTNode getShallowClone() {
		return new FSTTerminal(getType(), getName(), getBody(),
				getSpecialTokenPrefix(), getCompositionMechanism(),
				getMergingMechanism(), getOriginalFeatureName());
	}

	@Override
	public FSTNode getDeepClone() {
		return new FSTTerminal(getType(), getName(), getBody(),
				getSpecialTokenPrefix(), getCompositionMechanism(),
				getMergingMechanism(), getOriginalFeatureName());
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

	public String getContractCompKey() {
		return contractCompKey;
	}

	public void setContractCompKey(String contractCompKey) {
		this.contractCompKey = contractCompKey;
	}

	public void setOriginalFeatureName(final String feature) {
		this.feature = feature;
	}
	
	public String getOriginalFeatureName() {
		return feature;
	}
}
