package cide.gast;

public class PropertyZeroOrOne<T extends ASTNode> extends Property {

	protected T value;

	public PropertyZeroOrOne(String name, T value) {
		super(name, PropertyType.ZEROORONE);
		this.value = value;
	}

	public T getValue() {
		return value;
	}

	public void setValue(T value) {
		this.value = value;
	}

	public boolean canRemoveSubtree(ASTNode node) {
		return value == node;
	}

	public void removeSubtree(ASTNode node) {
		if (value == node)
			setValue(null);
	}

	
	void setParent(ASTNode parent) {
		super.setParent(parent);
		if (value != null)
			value.setParent(parent, this);
	}

	public boolean hasValue() {
		return value != null;
	}

	Property deepCopy() {
		T newValue = null;
		if (value != null)
			newValue = (T) value.deepCopy();
		return new PropertyZeroOrOne<T>(new String(name), newValue);
	}

	public ASTNode[] getChildren() {
		if (value != null)
			return new ASTNode[] { value };
		else
			return new ASTNode[] {};
	}
}
