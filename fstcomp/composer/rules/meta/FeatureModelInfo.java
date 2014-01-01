package composer.rules.meta;

import java.util.List;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;



public interface FeatureModelInfo {

	public boolean isObligatory(String featureName,boolean useSelection);
	public boolean isObligatory(String featureName);

	public boolean isObligatoryForMethod(String className, String methodName, String featureName,boolean useSelection);
	
	public boolean isObligatoryForMethod(String className, String methodName, String featureName);
	
	/*
	
	@Deprecated
	public boolean hasValidProduct(List<String> selectedFeatures, List<String> rejectedFeatures);
	
	@Deprecated
	public boolean isFeatureImplied(String featureName, List<String> selectedFeatures, List<String> rejectedFeatures);
	
	@Deprecated
	public boolean isNotFeatureImplied(String featureName, List<String> selectedFeatures, List<String> rejectedFeatures);
	
	*/
	
	public void addFeatureNodes(List<FSTNonTerminal> features);
	
	public void clearFeatureNodes();
	
	public void selectFeature(String featureName);
	
	public void rejectFeature(String featureName);
	
	public void resetSelections();
	
	public void resetRejections();
	
	public void reset();
	
	public boolean isValidSelection();
	
	public boolean isSelectable(String featureName);
	
	public boolean isRejectable(String featureName);
	
	public boolean isSelectionImplied(String featureName);
	
	public boolean isRejectionImplied(String featureName);
}
