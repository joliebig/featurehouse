package de.ovgu.cide.fstgen.ast;

public abstract class FSTNode {
	private String name;
	private String type;

	protected FSTNode(String type, String name) {
		this.type = type;
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public String getType() {
		return type;
	}

	public abstract String printFST(int i) ;
	
	public abstract void accept(FSTVisitor visitor);
}
