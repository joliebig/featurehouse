package cide.greferences;

import cide.gast.ASTNode;

public class Reference implements IReference {

	private ASTNode target;
	private ASTNode source;
	private IReferenceType type;

	public Reference(IReferenceType referenceType, ASTNode source,
			ASTNode target) {
		this.type = referenceType;
		this.source = source;
		this.target = target;
	}

	public ASTNode getSource() {
		return source;
	}

	public ASTNode getTarget() {
		return target;
	}

	public IReferenceType getType() {
		return type;
	}

}
