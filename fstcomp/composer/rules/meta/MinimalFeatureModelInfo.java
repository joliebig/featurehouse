package composer.rules.meta;

import java.util.List;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class MinimalFeatureModelInfo implements FeatureModelInfo {

	@Override
	public boolean isObligatory(String featureName) {
		return false;
	}

	@Override
	public boolean isObligatoryForMethod(String className, String methodName,
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
	public void rejectFeature(String featureName) {
		
	}

	@Override
	public void resetSelections() {
		
	}

	@Override
	public void resetRejections() {
		
	}

	@Override
	public void reset() {
		
	}

	@Override
	public boolean isValidSelection() {
		return true;
	}

	@Override
	public boolean isSelectable(String featureName) {
		return true;
	}

	@Override
	public boolean isRejectable(String featureName) {
		return true;
	}

	@Override
	public boolean isSelectionImplied(String featureName) {
		return false;
	}

	@Override
	public boolean isRejectionImplied(String featureName) {
		return false;
	}

	@Override
	public boolean isObligatory(String featureName, boolean useSelection) {
		return false;
	}

	@Override
	public boolean isObligatoryForMethod(String className, String methodName,
			String featureName, boolean useSelection) {
		return false;
	}

}
