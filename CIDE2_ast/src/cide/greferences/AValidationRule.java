package cide.greferences;

import java.util.Set;

import cide.features.IASTColorProvider;
import cide.features.IFeature;
import cide.gast.ASTNode;

public abstract class AValidationRule implements IValidationRule {

	public int getErrorSeverity() {
		return Error;
	}

	protected boolean isColorSubset(IASTColorProvider colorProvider,
			ASTNode subset, ASTNode superset) {
		Set<IFeature> subsetColors = colorProvider.getColorsI(subset);
		Set<IFeature> supersetColors = colorProvider.getColorsI(superset);
		return supersetColors.containsAll(subsetColors);
	}

}
