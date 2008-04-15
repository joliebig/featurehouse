package de.ovgu.cide.fstgen.ast;

import java.util.LinkedList;

public class FSTTerminal extends FSTNode {

	private String body;
	private String compose = "replacement";

	public FSTTerminal(String type, String name, String body) {
		super(type, name);
		this.body = body;
	}

	public FSTTerminal(String type, String name, String body,
			String compositionMechanism) {
		this(type, name, body);
		this.compose = compositionMechanism;
	}
	
	@Override
	public FSTNode getShallowClone() {
		return new FSTTerminal(getType(), getName(), getBody(), getCompositionMechanism());
	}

	@Override
	public FSTNode getDeepClone() {
		return new FSTTerminal(getType(), getName(), getBody(), getCompositionMechanism());
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
		return "[T -> " + getName() + " : " + getType() + " \""
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
