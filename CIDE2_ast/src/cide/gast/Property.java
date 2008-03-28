package cide.gast;

public abstract class Property {
	protected final String name;

	protected final PropertyType type;

	protected ASTNode parent;

	public enum PropertyType {
		ONE, ZEROORONE, ZEROORMORE, ONEORMORE, LIST
	}

	public Property(String name, PropertyType type) {
		this.name = name;
		this.type = type;
	}

	public abstract boolean canRemoveSubtree(ASTNode node);

	public abstract void removeSubtree(ASTNode node);

	public abstract ASTNode[] getChildren();

	void setParent(ASTNode node) {
		this.parent = node;
	}

	public ASTNode getNode() {
		return parent;
	}

	public String getName() {
		return name;
	}

	public PropertyType getType() {
		return type;
	}

	/**
	 * generates part of the ASTNode's id
	 * 
	 * @param node
	 * @return
	 */
	String getId(ASTNode node) {
		return name;
	}

	abstract Property deepCopy();

	public boolean isWrapper() {
		return false;
	}
}
