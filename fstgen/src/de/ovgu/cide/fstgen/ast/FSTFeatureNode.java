package de.ovgu.cide.fstgen.ast;

import java.util.LinkedList;

public class FSTFeatureNode extends FSTNonTerminal {
	public FSTFeatureNode(String name) {
		super("Feature", name);
	}
	public boolean compatibleWith(FSTNode node) {
		return this.getType().equals(node.getType());
	}
	
    public FSTNode getShallowClone() {
    	return new FSTFeatureNode(getName());
    }
}
