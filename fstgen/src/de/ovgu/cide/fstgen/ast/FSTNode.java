package de.ovgu.cide.fstgen.ast;

public abstract class FSTNode {
	private String name;
	private String type;
	private FSTNode parent = null;
	public int index = -1;

	protected FSTNode(String type, String name) {
		this.setType(type);
		this.setName(name);
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getType() {
		return type;
	}

	public void setParent(FSTNode parent) {
		this.parent = parent;
	}

	public FSTNode getParent() {
		return parent;
	}
	
	public boolean compatibleWith(FSTNode node) {
		return this.getType().equals(node.getType()) && this.getName().equals(node.getName());
	}
	
	public abstract FSTNode getShallowClone() ;
	public abstract FSTNode getDeepClone() ;
	
	public abstract String printFST(int i) ;
	
	public abstract void accept(FSTVisitor visitor);
}
