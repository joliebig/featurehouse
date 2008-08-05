package cide.gast;

import java.util.ArrayList;

public class PropertyOneOrMore<T extends ASTNode> extends PropertyZeroOrMore<T> {

	public PropertyOneOrMore(String name, ArrayList<T> value) {
		super(name, value);
	}

	public void removeSubtree(ASTNode value) {
		if (this.valueList.indexOf(value) != 0)
			super.removeSubtree(value);
	}

	public boolean canRemoveSubtree(ASTNode node) {
		return this.valueList.indexOf(node) != 0;
	}

	/**
	 * after cloning the IDs might change (due to renumbering) but are again
	 * consistent inside the AST
	 */
	Property deepCopy() {
		ArrayList<T> clonedList = new ArrayList<T>(valueList.size());
		for (T entry : valueList)
			clonedList.add((T) entry.deepCopy());
		return new PropertyOneOrMore<T>(new String(name), clonedList);
	}

}
