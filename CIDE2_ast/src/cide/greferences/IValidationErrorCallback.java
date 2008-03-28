package cide.greferences;

import cide.gast.ASTNode;

public interface IValidationErrorCallback {
	public void validationError(ASTNode node, IValidationRule brokenRule);
}
