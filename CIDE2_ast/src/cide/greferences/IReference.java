package cide.greferences;

import cide.gast.ASTNode;

public interface IReference {

	ASTNode getSource();

	ASTNode getTarget();

	IReferenceType getType();

}
