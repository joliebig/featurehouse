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
		original.rejectFeature(featureName);
	}
	
	public void clear(){
		selected.clear();
		rejected.clear();
		reset();
	}
	
	@Override
	public boolean isObligatory(String featureName) {
		if (selected.contains(featureName))
			return true;
		if (rejected.contains(featureName))
			return false;
		reset();
		return original.isObligatory(featureName,true);
	}
	
	@Override
	public boolean isObligatory(String featureName,boolean useSelection) {
		if (selected.contains(featureName))
			return true;
		if (rejected.contains(featureName))
			return false;
		return original.isObligatory(featureName,useSelection);
	}

	@Override
	public boolean isObligatoryForMethod(String className, String methodName,
			String featureName) {
		if (selected.contains(featureName))
			return true;
		if (rejected.contains(featureName))
			return false;
		reset();
		return original.isObligatoryForMethod(className, methodName, featureName, true);
	}

	@Override
	public boolean isObligatoryForMethod(String className, String methodName,
			String featureName,boolean useSelection) {
		if (selected.contains(featureName))
			return true;
		if (rejected.contains(featureName))
			return false;
		return original.isObligatoryForMethod(className, methodName, featureName, useSelection);
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
	public void rejectFeature(String featureName) {
		if (selected.contains(featureName))
			invalidRejection = true;
		original.rejectFeature(featureName);
		
	}

	@Override
	public void resetSelections() {
		original.resetSelections();
		for (String feature : selected)
			original.selectFeature(feature);
		invalidSelection = false;
	}

	@Override
	public void resetRejections() {
		original.resetSelections();
		for (String feature : rejected)
			original.rejectFeature(feature);
		invalidRejection = false;
	}

	@Override
	public void reset() {
		original.reset();
		for (String feature : selected)
			original.selectFeature(feature);
		for (String feature : rejected)
			original.rejectFeature(feature);
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
	public boolean isSelectable(String featureName) {
		if (invalidSelection || invalidRejection)
			return false;
		if (rejected.contains(featureName))
			return true;
		return original.isSelectable(featureName);
	}

	@Override
	public boolean isRejectable(String featureName) {
		if (invalidSelection || invalidRejection)
			return false;
		if (selected.contains(featureName))
			return true;
		return original.isRejectable(featureName);
	}

	@Override
	public boolean isSelectionImplied(String featureName) {
		if (selected.contains(featureName))
			return true;
		return original.isSelectionImplied(featureName);
	}

	@Override
	public boolean isRejectionImplied(String featureName) {
		if (rejected.contains(featureName))
			return true;
		return original.isRejectable(featureName);
	}

}
