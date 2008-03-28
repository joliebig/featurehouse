package cide.greferences;

import java.util.HashMap;
import java.util.Set;

import cide.features.IASTColorProvider;
import cide.gast.ASTNode;

public abstract class ASimpleSubsetValidationRule extends AValidationRule {

	private boolean targetIsSubset;
	private IReferenceType type;

	protected ASimpleSubsetValidationRule(IReferenceType type,
			boolean targetIsSubset) {
		this.type = type;
		this.targetIsSubset = targetIsSubset;
	}

	public IReferenceType[] getRequiredReferences() {
		return new IReferenceType[] { type };
	}

	public void validate(IASTColorProvider colorProvider,
			HashMap<IReferenceType, Set<IReference>> allReferences,
			IValidationErrorCallback errorCallback) {
		Set<IReference> references = allReferences.get(type);
		for (IReference reference : references) {
			ASTNode source = reference.getSource();
			ASTNode target = reference.getTarget();
			boolean result;
			if (targetIsSubset)
				result = super.isColorSubset(colorProvider, target, source);
			else
				result = super.isColorSubset(colorProvider, source, target);
			if (!result)
				errorCallback.validationError(targetIsSubset ?  source:target,
						this);
		}
	}

}
