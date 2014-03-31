package composer.rules.meta;

import java.util.List;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class MinimalFeatureModelInfo implements FeatureModelInfo {

	@Override
	public boolean isCoreFeature(String featureName) {
		return false;
	}

	@Override
	public boolean isMethodCoreFeature(String className, String methodName,
			String featureName) {
		return false;
	}

	@Override
	public void addFeatureNodes(List<FSTNonTerminal> features) {
		
	}

	@Override
	public void clearFeatureNodes() {
		
	}

	@Override
	public void selectFeature(String featureName) {
		
	}

	@Override
	public void eliminateFeature(String featureName) {
		
	}

	@Override
	public void resetSelections() {
		
	}

	@Override
	public void resetEliminations() {
		
	}

	@Override
	public void reset() {
		
	}

	@Override
	public boolean isValidSelection() {
		return true;
	}

	@Override
	public boolean canBeSelected(String featureName) {
		return true;
	}

	@Override
	public boolean canBeEliminated(String featureName) {
		return true;
	}

	@Override
	public boolean isAlwaysSelected(String featureName) {
		return false;
	}

	@Override
	public boolean isAlwaysEliminated(String featureName) {
		return false;
	}

	@Override
	public boolean isCoreFeature(String featureName, boolean useSelection) {
		return false;
	}

	@Override
	public boolean isMethodCoreFeature(String className, String methodName,
			String featureName, boolean useSelection) {
		return false;
	}

	@Override
	public String getValidClause() {
		return "FM.FeatureModel.valid()";
	}

}
