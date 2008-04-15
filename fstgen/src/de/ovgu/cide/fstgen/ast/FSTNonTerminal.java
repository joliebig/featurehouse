package de.ovgu.cide.fstgen.ast;

import java.util.LinkedList;
import java.util.List;

public class FSTNonTerminal extends FSTNode {

	private List<FSTNode> children;

	public FSTNonTerminal(String type, String name, List<FSTNode> children) {
		super(type, name);
		this.children = children;
	}
	
	@Override
	public FSTNode clone() {
		return new FSTNonTerminal(getType(), getName(), new LinkedList<FSTNode>());
	}
	
	public List<FSTNode> getChildren() {
		return children;
	}

	public void addChild(FSTNode child) {
		child.setParent(this);
		children.add(child);
	}
	
	public FSTNode getCompatibleChild(FSTNode node) {
		for(FSTNode child : getChildren()) {
			if(child.compatibleWith(node))
				return child;
		}
		return null;
	}
	
	public String toStringShort() {
		return "[N: " + getType() + "/" + getName() + "]";
	}

	@Override
	public String toString() {
		String shortS = toStringShort();
		return shortS.substring(0, shortS.length() - 1) + " "
				+ printChildrenList() + "]";
	}

	private String printChildrenList() {
		String result = "[";
		for (int idx = 0; idx < children.size(); idx++) {
			if (idx != 0)
				result += ", ";
			result += children.get(idx).getType() + "/"
					+ children.get(idx).getName();
		}
		return result + "]";
	}

	public String printFST(int indent) {
		StringBuffer buffer = new StringBuffer();
		for (int i = 0; i < indent; i++)
			buffer.append("    ");
		buffer.append(this.toStringShort());
		buffer.append("\n");
		for (FSTNode n : children) {
			buffer.append(n.printFST(indent + 1));
		}
		return buffer.toString();
	}

	@Override
	public void accept(FSTVisitor visitor) {
		boolean visitInner = visitor.visit(this);
		if (visitInner)
			for (FSTNode child : children)
				child.accept(visitor);
		visitor.postVisit(this);
	}

}
