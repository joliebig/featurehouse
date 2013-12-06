package composer.rules.meta;

import java.util.List;


public interface FeatureModelInfo {

	/*
	 * erhält zwei Listen mit Feature-Namen:
	 * erste Liste enthält selektierte Features
	 * zweite Liste enthält Features, die definitiv nicht selektiert sind
	 * dritte Liste enthält Features, für die die Auswahl irrelevant ist
	 * 
	 * Ergebnis: true, wenn es eine gültige Kombination der Fehlenden Features gibt, sodass ein 
	 * 					valides Produkt entsteht
	 * 			 false, sonst
	 */
	public boolean possibleValid(List<String> selectedFeatures, List<String> unselectedFeatures, List<String> irrelevantFeatures);
	
	/*
	 * erhält zwei Feature-Namen
	 * liefert true, wenn kein Produkt beide Features enthalten kann, sonst false
	 */
	public boolean isComplementFeature(String firstFeature, String secondFeature);
	
	/*
	 * erhält List mit Features
	 * liefert true, wenn Liste ein AtomicSet ist (Features immer zusammen vorhanden sein müssen)
	 */
	public boolean isAtomicSet(List<String> featureList);
	
}
