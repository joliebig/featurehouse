package cide.gast;

import java.util.ArrayList;
import java.util.List;

public class PropertyZeroOrMore<T extends ASTNode> extends Property {

	protected final ArrayList<T> valueList;
	/**
	 * the indexlist keeps a list of all values including removed ones so that
	 * the generated ID is permanent and does not change if a value is removed
	 */
	protected final List<T> indexList = new ArrayList<T>();

	public PropertyZeroOrMore(String name, ArrayList<T> value) {
		this(name, value, PropertyType.ZEROORMORE);
	}

	protected PropertyZeroOrMore(String name, ArrayList<T> value, PropertyType type) {
		super(name, type);
		this.valueList = value;
		this.indexList.addAll(value);
	}

	public ArrayList<T> getValue() {
		return /*Collections.unmodifiableList*/(valueList);
	}

//	public void addValue(T value) {
//		this.valueList.add(value);
//		this.indexList.add(value);
//	}
//
//	public void removeValue(T value) {
//		this.valueList.remove(value);
//	}

	public boolean canRemoveSubtree(ASTNode node) {
		return valueList.contains(node);
	}

	public void removeSubtree(ASTNode node) {
		this.valueList.remove(node);
	}

	void setParent(ASTNode parent) {
		super.setParent(parent);
		for (T value : valueList)
			value.setParent(parent, this);
	}

	String getId(ASTNode node) {
		return super.getId(node) + indexList.indexOf(node);
	}

	/**
	 * after cloning the IDs might change (due to renumbering) but are again
	 * consistent inside the AST
	 */
	Property deepCopy() {
		ArrayList<T> clonedList = new ArrayList<T>(valueList.size());
		for (T entry : valueList)
			clonedList.add((T) entry.deepCopy());
		return new PropertyZeroOrMore<T>(new String(name), clonedList);
	}

	public ASTNode[] getChildren() {
		return valueList.toArray(new ASTNode[valueList.size()]);
	}

}
