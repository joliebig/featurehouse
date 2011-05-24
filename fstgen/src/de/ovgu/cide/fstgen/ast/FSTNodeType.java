package de.ovgu.cide.fstgen.ast;

public class FSTNodeType {

	private String name;
	private boolean isTerminal;

	public FSTNodeType(String name, boolean isTerminal) {
		this.name = name;
		this.isTerminal = isTerminal;
	}

	public String getName() {
		return name;
	}

	public boolean isTerminal() {
		return isTerminal;
	}

}
