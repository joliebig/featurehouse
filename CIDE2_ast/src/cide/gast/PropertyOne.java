package cide.gast;

public class PropertyOne<T extends ASTNode> extends Property {

	protected T value;

	public PropertyOne(String name, T value) {
		super(name, PropertyType.ONE);
		this.value = value;
	}

	public T getValue() {
		return value;
	}

	public void setValue(T value) {
		if (value != null)
			this.value = value;
	}

	public boolean canRemoveSubtree(ASTNode node) {
		return false;
	}

	public void removeSubtree(ASTNode node) {
		throw new UnsupportedOperationException();
	}

	void setParent(ASTNode parent) {
		super.setParent(parent);
		value.setParent(parent, this);
	}

	Property deepCopy() {
		return new PropertyOne<T>(new String(name), (T) value.deepCopy());
	}

	public ASTNode[] getChildren() {
		return new ASTNode[] { value };
	}
}
