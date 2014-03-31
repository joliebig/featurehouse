package composer.rules.meta;

import java.util.List;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;



public interface FeatureModelInfo {

	public boolean isCoreFeature(String featureName,boolean useSelection);
	public boolean isCoreFeature(String featureName);

	public boolean isMethodCoreFeature(String className, String methodName, String featureName,boolean useSelection);
	
	public boolean isMethodCoreFeature(String className, String methodName, String featureName);
	
	public void addFeatureNodes(List<FSTNonTerminal> features);
	
	public void clearFeatureNodes();
	
	public void selectFeature(String featureName);
	
	public void eliminateFeature(String featureName);
	
	public void resetSelections();
	
	public void resetEliminations();
	
	public void reset();
	
	public boolean isValidSelection();
	
	public boolean canBeSelected(String featureName);
	
	public boolean canBeEliminated(String featureName);
	
	public boolean isAlwaysSelected(String featureName);
	
	public boolean isAlwaysEliminated(String featureName);
	
	public String getValidClause();
}
