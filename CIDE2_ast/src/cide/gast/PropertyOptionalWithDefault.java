package cide.gast;

/**
 * optional value that is replaced by a string for print-visitors if removed.
 * 
 */
public class PropertyOptionalWithDefault<T extends ASTNode> extends Property {

	protected T value;
	private final String defaultValue;
	private final ASTStringNode defaultNode;

	public PropertyOptionalWithDefault(String name, T value, String defaultValue) {
		super(name, PropertyType.ONE);
		this.value = value;
		this.defaultValue = defaultValue;
		defaultNode = new ASTStringNode(defaultValue, genToken());
	}

	private IToken genToken() {
		return new IToken() {
			public int getLength() {
				return 0;
			}

			public int getOffset() {
				return -1;
			}
		};
	}

	public ASTNode getValue() {
		if (value == null)
			return defaultNode;
		return value;
	}

	public void setValue(T value) {
		this.value = value;
	}

	public boolean canRemoveSubtree(ASTNode node) {
		return node == value;
	}

	public void removeSubtree(ASTNode node) {
		if (node == value)
			value = null;
	}


	void setParent(ASTNode parent) {
		super.setParent(parent);
		value.setParent(parent, this);
	}

	Property deepCopy() {
		return new PropertyOptionalWithDefault<T>(new String(name), (T) value
				.deepCopy(), new String(defaultValue));
	}
	
	public ASTNode[] getChildren() {
		if (value!=null)
			return new ASTNode[] { value };
		else
			return new ASTNode[] { defaultNode };
	}
}
