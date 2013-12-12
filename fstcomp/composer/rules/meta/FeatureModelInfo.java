package composer.rules.meta;

import java.util.List;


public interface FeatureModelInfo {

	public boolean isObligatory(String featureName);
	
	public boolean hasValidProduct(List<String> selectedFeatures, List<String> rejectedFeatures);
	
}
