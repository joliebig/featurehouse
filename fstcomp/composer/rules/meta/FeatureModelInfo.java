package composer.rules.meta;

import java.util.List;


public interface FeatureModelInfo {

	public boolean isObligatory(String featureName);
	
	public boolean isObligatoryForMethod(String methodName, String featureName);
	
	public boolean hasValidProduct(List<String> selectedFeatures, List<String> rejectedFeatures);
	
	public boolean isFeatureImplied(String featureName, List<String> selectedFeatures, List<String> rejectedFeatures);
	
	public boolean isNotFeatureImplied(String featureName, List<String> selectedFeatures, List<String> rejectedFeatures);
	
}
