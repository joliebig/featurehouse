package composer.rules.meta;

import java.util.HashSet;
import java.util.List;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class MethodBasedModelInfoWrapper implements FeatureModelInfo {

	FeatureModelInfo original;
	HashSet<String> selected = new HashSet<String>();
	HashSet<String> rejected = new HashSet<String>();
	boolean invalidSelection = false;
	boolean invalidRejection = false;
	
	public MethodBasedModelInfoWrapper(FeatureModelInfo originalModelInfo){
		original = originalModelInfo;
	}
	
	public void setSelected(String featureName){
		selected.add(featureName);
		original.selectFeature(featureName);
	}
	
	public void setRejected(String featureName){
		rejected.add(featureName);
		original.eliminateFeature(featureName);
	}
	
	public void clear(){
		selected.clear();
		rejected.clear();
		reset();
	}
	
	@Override
	public boolean isCoreFeature(String featureName) {
		if (selected.contains(featureName))
			return true;
		if (rejected.contains(featureName))
			return false;
		reset();
		return original.isCoreFeature(featureName,true);
	}
	
	@Override
	public boolean isCoreFeature(String featureName,boolean useSelection) {
		if (selected.contains(featureName))
			return true;
		if (rejected.contains(featureName))
			return false;
		return original.isCoreFeature(featureName,useSelection);
	}

	@Override
	public boolean isMethodCoreFeature(String className, String methodName,
			String featureName) {
		if (selected.contains(featureName))
			return true;
		if (rejected.contains(featureName))
			return false;
		reset();
		return original.isMethodCoreFeature(className, methodName, featureName, true);
	}

	@Override
	public boolean isMethodCoreFeature(String className, String methodName,
			String featureName,boolean useSelection) {
		if (selected.contains(featureName))
			return true;
		if (rejected.contains(featureName))
			return false;
		return original.isMethodCoreFeature(className, methodName, featureName, useSelection);
	}

	@Override
	public void addFeatureNodes(List<FSTNonTerminal> features) {
		original.addFeatureNodes(features);
	}

	@Override
	public void clearFeatureNodes() {
		original.clearFeatureNodes();		
	}

	@Override
	public void selectFeature(String featureName) {
		if (rejected.contains(featureName))
			invalidSelection = true;
		original.selectFeature(featureName);
	}

	@Override
	public void eliminateFeature(String featureName) {
		if (selected.contains(featureName))
			invalidRejection = true;
		original.eliminateFeature(featureName);
		
	}

	@Override
	public void resetSelections() {
		original.resetSelections();
		for (String feature : selected)
			original.selectFeature(feature);
		invalidSelection = false;
	}

	@Override
	public void resetEliminations() {
		original.resetSelections();
		for (String feature : rejected)
			original.eliminateFeature(feature);
		invalidRejection = false;
	}

	@Override
	public void reset() {
		original.reset();
		for (String feature : selected)
			original.selectFeature(feature);
		for (String feature : rejected)
			original.eliminateFeature(feature);
		invalidSelection = false;
		invalidRejection = false;
	}

	@Override
	public boolean isValidSelection() {
		if (invalidSelection || invalidRejection)
			return false;
		return original.isValidSelection();
	}

	@Override
	public boolean canBeSelected(String featureName) {
		if (invalidSelection || invalidRejection)
			return false;
		if (rejected.contains(featureName))
			return true;
		return original.canBeSelected(featureName);
	}

	@Override
	public boolean canBeEliminated(String featureName) {
		if (invalidSelection || invalidRejection)
			return false;
		if (selected.contains(featureName))
			return true;
		return original.canBeEliminated(featureName);
	}

	@Override
	public boolean isAlwaysSelected(String featureName) {
		if (selected.contains(featureName))
			return true;
		return original.isAlwaysSelected(featureName);
	}

	@Override
	public boolean isAlwaysEliminated(String featureName) {
		if (rejected.contains(featureName))
			return true;
		return original.isAlwaysEliminated(featureName);
	}

	@Override
	public String getValidClause() {
		return "FM.FeatureModel.valid()";
	}

}
