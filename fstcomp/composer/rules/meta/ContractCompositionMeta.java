package composer.rules.meta;



import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import metadata.CompositionMetadataStore;

import composer.CompositionException;
import composer.FSTGenComposerExtension;
import composer.rules.CompositionKeyword;
import composer.rules.ContractComposition;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

/**
 * TODO description
 * @author Jens Meinicke
 *
 */
public class ContractCompositionMeta extends ContractComposition {

	private FeatureModelInfo modelInfo;
	private CompositionMetadataStore metadata = CompositionMetadataStore.getInstance();
		
	public ContractCompositionMeta(String contract_style) {
		super(contract_style);
	}
	
	public ContractCompositionMeta(String contract_style, FeatureModelInfo model){
		super(contract_style);
		this.modelInfo = model;
	}
	
	public void setFeatureModelInfo(FeatureModelInfo model){
		this.modelInfo = model;
	}
	
	private String stateToFeatureName(String state){
		List<String> features = metadata.getFeatures();
		for (String feature : features){
			if (feature.toLowerCase().equals(state))
				return feature;
		}
		return state;
	}

	private List<String> getSelectedFeatures(String clause){
		Pattern featurePattern = Pattern.compile("[^!]FM\\.FeatureModel\\.([\\w]*)" + (FSTGenComposerExtension.key ? "" : "\\(\\)") +" ==>");
		Matcher featureMatcher = featurePattern.matcher(" " + clause);
		List<String> featureList = new ArrayList<String>();
		while (featureMatcher.find()){
			featureList.add(stateToFeatureName(featureMatcher.group(1)));
		}
		return featureList;
	}
	
	private List<String> getRejectedFeatures(String clause){
		Pattern featurePattern = Pattern.compile("!FM\\.FeatureModel\\.([\\w]*)" + (FSTGenComposerExtension.key ? "" : "\\(\\)") +" ==>");
		Matcher featureMatcher = featurePattern.matcher(clause);
		List<String> featureList = new ArrayList<String>();
		while (featureMatcher.find()){
			featureList.add(stateToFeatureName(featureMatcher.group(1)));
		}
		return featureList;
	}

	@Override
	protected String getReplacementString(FSTTerminal terminalA,
			FSTTerminal terminalB) {
		String res = super.getReplacementString(terminalA, terminalB);
		String[] baseCases = terminalB.getBody().trim().split("also");
		String orOriginal = "";
		for (String caseB : baseCases) {
			String[] clasesB = caseB.trim().split(";");
			for (String clauseB : clasesB) {
				if (!clauseB.trim().isEmpty()) {
					if (clauseB.contains(ORIGINAL_OR)) {
						orOriginal = clauseB.substring(clauseB.indexOf("\\req "));
						orOriginal = orOriginal.replaceAll("\\\\req ", "");
						clauseB = clauseB.substring(0, clauseB.indexOf("\\req "));
						res += clauseB;
					} else {
						clauseB = clauseB.replaceAll("requires ", "requires !FM.FeatureModel." + getFeatureState(terminalA) + " ==> ");
						clauseB = clauseB.replaceAll("ensures ", "ensures !FM.FeatureModel." + getFeatureState(terminalA) + " ==> ");
						if (clauseB.contains("requires ")) {
							res = res.replaceFirst("requires ", Matcher.quoteReplacement(clauseB) + ";\nrequires ");
						} else if (clauseB.contains("ensures ")) {
							res = res.replaceFirst("ensures ", Matcher.quoteReplacement(clauseB) + ";\nensures ");
						} else {
							res += clauseB + ";";
						}
					}
				}
			}
		}
		res = res.toString().replaceAll("\r", "");
		res = res.replaceAll("@", "");
		res = res.replaceAll("\\n", "");
		res = res.replaceAll("\\t", "");
		while (res.contains("  ")) {
			res = res.replaceAll("  ", " ");
		}
		String[] allClauses = res.split(";");
		StringBuilder result = new StringBuilder();
		for (int i  = 0; i < allClauses.length;i++) {
			result.append(allClauses[i].replaceAll("\\t", " ").replaceAll("  ", " ").trim());
			result.append(";\r\n\t");
			if (i < allClauses.length - 1) {
				result.append(" @ ");
			}
		}
		return result.toString().replace(ORIGINAL_OR, " || " + orOriginal.replaceAll("@", ""));
	}
	
	private static String getFeatureState(FSTNode node) {
		return getFeatureName(node).toLowerCase() + (FSTGenComposerExtension.key ? "" : "()");
	}
	
	private static String getFeatureName(FSTNode node) {
		if ("Feature".equals(node.getType()))
			return node.getName();
		else
			return getFeatureName(node.getParent());
	}

	@Override
	public void compose(FSTTerminal terminalA, FSTTerminal terminalB,
			FSTTerminal terminalComp, FSTNonTerminal nonterminalParent)
			throws CompositionException {
		
		if (terminalB.getBody().contains("notcomposed")){
			FSTTerminal newComp = (FSTTerminal)terminalComp.getDeepClone();
			FSTTerminal newB = (FSTTerminal)terminalB.getDeepClone();
			newB.setBody("");
			compose(terminalB,newB,newComp,nonterminalParent);
			terminalB = newComp;
		}

		String body = terminalA.getBody();
		body = body.replaceAll("notcomposed","");
		terminalA.setBody(body);
		super.compose(terminalA,terminalB,terminalComp,nonterminalParent);
	}
	
	
	/**
	 * TODO description
	 */
	@Override
	public void preCompose(FSTTerminal terminal) {
		// TODO
		
		String body = terminal.getBody();
		terminal.setBody("notcomposed" + body);
		return;
		/*
		String body = terminal.getBody();
		body = body.replaceAll("\t", "");
		body = body.replaceAll("\n", "\n\t @ ");
		while (body.contains("  ")) {
			body = body.replaceAll("  ", " ");
		}

		String result = "";
		boolean added = false;
		boolean open = false;
		if (body.contains(";")) {
			for (String s : body.split(";")) {
				if (s.contains("ensures ")) {
					if (open) {
						result += ")";
						open = false;
					}
					if (added) {
						result += ";";
					}
					result = result + s.replace("ensures ", "ensures FM.FeatureModel." + getFeatureState(terminal) + " ==> (");
					open = true;
				} else if (s.contains("requires ")) {
					if (open) {
						result += ")";
						open = false;
					}
					if (added) {
						result += ";";
					}
					result = result + s.replace("requires ", "requires FM.FeatureModel." + getFeatureState(terminal) + " ==> (");
					open = true;
				} else if (s.contains("assignable ")) {
					if (open) {
						result += ")";
						open = false;
					}
					if (added) {
						result += ";";
					}
					result = result + s;
				} else {
					if (open) {
						result += ")";
						open = false;
					}
					if (added) {
						result += ";";
					}
					result = result + s;
				}
				added = true;
			}
			if (open) {
				result += ")";
			}
			if (added) {
				result += ";";
			}
			body = result;
		}
	
		int ensuresIndex = body.indexOf("ensures ");
		int requiresIndex = body.indexOf("requires ");
		int index = ensuresIndex < requiresIndex ? ensuresIndex : requiresIndex;
		if (ensuresIndex == -1) {
			if (requiresIndex == -1) {
				index = 0;
			} else {
				index = requiresIndex;
			}
		} else if (requiresIndex == -1) {
			index = ensuresIndex;
		}
		
		String start = body.substring(0, index);
		String end = body.substring(index);
		terminal.setBody(start + "\\req FM.FeatureModel." + getFeatureState(terminal) + ORIGINAL_OR + ";\r\n\t @ " + end); */
	}
	 
	/**
	 * TODO description
	 */
	@Override
	public void postCompose(FSTTerminal terminal) {
		// TODO
		String body = terminal.getBody();
		if (FSTGenComposerExtension.key && body.replaceAll("@", "").replaceAll("\\\\[req,nreq][^;]*;", "").trim().isEmpty()) {
			terminal.setBody("");
			return;
		}
		
		body = body.replaceAll("\\\\req", "requires");
		body = body.replaceAll("\\\\nreq", "requires");
		body = body.replaceAll("\\" + ORIGINAL_OR, "");
		body = body.replaceAll("\\" + ORIGINAL_KEYWORD, "true");
		if (FSTGenComposerExtension.key) {
			body = "\r\n\t @ requires FM.FeatureModel.valid();\r\n\t @ " + body;
		} else {
			body = "\r\n\t @ " + body;
		}
		
		if (!body.endsWith("\r\n\t")) {
			body = body + "\r\n\t";
		}
		terminal.setBody(body);
	}
	
	protected void compositionByKeywords(FSTTerminal terminalA,
			FSTTerminal terminalB, FSTTerminal terminalComp,
			FSTNonTerminal nonterminalParent) {
		// TODO
		String compositionKey = "";

		for (FSTNode n : ((FSTNonTerminal) terminalB.getParent()).getChildren()) {
			if (n.getType().equals("ContractCompKey")) {
				compositionKey = ((FSTTerminal) n).getContractCompKey();
				break;
			}
		}	

		if (compositionKey.equals(CompositionKeyword.FINAL_CONTRACT.getLabel())
				|| compositionKey.equals(CompositionKeyword.FINAL_METHOD
						.getLabel())) {
			plainContracting(terminalA, terminalB, terminalComp);
		} else if (compositionKey
				.equals(CompositionKeyword.CONSECUTIVE_CONTRACT.getLabel())) {
			consecutiveContracting(terminalA, terminalB, terminalComp);
		} else if (compositionKey
				.equals(CompositionKeyword.CONJUNCTIVE_CONTRACT.getLabel())) {
			conjunctiveContracting(terminalA, terminalB, terminalComp);
		} else if (compositionKey.equals(CompositionKeyword.CUMULATIVE_CONTRACT
				.getLabel())) {
			cumulativeContracting(terminalA, terminalB, terminalComp);
		} else {
			explicitContracting(terminalA, terminalB, terminalComp);
		}
	}

	@Override
	protected void plainContracting(FSTTerminal terminalA, FSTTerminal terminalB,
			FSTTerminal terminalComp) {
		// TODO: standardmäßig den Feature-Namen mit Implikation dazu packen
		//       evtl. besonderes internees Schlüsselwort verwenden, wenn der Kontrakt nicht überschrieben werden darf (weil durch 
		//		 obligarisches Feature gesetzt) ansonsten 
		terminalComp.setBody(terminalB.getBody());
	}

	protected void contractOverriding(FSTTerminal terminalA,
			FSTTerminal terminalB, FSTTerminal terminalComp) {
		
		String featureNameA = getFeatureName(terminalA);
		String featureStateA = getFeatureState(terminalA);
		
		if (modelInfo.isObligatory(featureNameA)){
			// overrides in every product
			terminalComp.setBody(terminalA.getBody());
		} else {
			// convert also, otherwise resulting contracts would not be correct
			if (terminalA.getBody().contains("also")){
				desugarAlso(terminalA);
			}
			if (terminalB.getBody().contains("also")){
				desugarAlso(terminalB);
			}
			// overrides only if Feature is selected			
			List<FSTTerminal> reqClaB = getRequiresClauses(terminalB);
			List<FSTTerminal> reqClaA = getRequiresClauses(terminalA);
			List<FSTTerminal> ensClaB = getEnsuresClauses(terminalB);
			List<FSTTerminal> ensClaA = getEnsuresClauses(terminalA);
			
			String terminalCompBody = "";
			
			// old requires-clauses
			for (FSTTerminal requiresB : reqClaB){
				List<String> selected = getSelectedFeatures(requiresB.getBody());
				List<String> rejected = getRejectedFeatures(requiresB.getBody());
				
				rejected.add(featureNameA);				
				if (modelInfo.hasValidProduct(selected, rejected)){
					// only add if combination could be possible (allredy selected and rejected Features plus reject Feature of TerminalB can lead to a valid Product)
					terminalCompBody += "\r\n\t requires !FM.FeatureModel." + featureStateA + " ==> (" + requiresB.getBody() + ");";
				}
			}
			
			// new requires-clauses
			for (FSTTerminal requiresA : reqClaA){
				terminalCompBody += "\r\n\t requires FM.FeatureModel." + featureStateA + " ==> (" + requiresA.getBody() + ");";
			}
			
			// old ensures-clauses
			for (FSTTerminal ensuresB : ensClaB){
				List<String> selected = getSelectedFeatures(ensuresB.getBody());
				List<String> rejected = getRejectedFeatures(ensuresB.getBody());
				
				rejected.add(featureNameA);				
				if (modelInfo.hasValidProduct(selected, rejected)){
					// only add if combination could be possible (allredy selected and rejected Features plus reject Feature of TerminalB can lead to a valid Product)
					terminalCompBody += "\r\n\t requires !FM.FeatureModel." + featureStateA + " ==> (" + ensuresB.getBody() + ");";
				}
			}
			
			// new ensures-clauses
			for (FSTTerminal ensuresA : ensClaA){
				terminalCompBody += "\r\n\t requires FM.FeatureModel." + featureStateA + " ==> (" + ensuresA.getBody() + ");";
			}
			
			terminalComp.setBody(terminalCompBody);
		}
		
	}

	protected void explicitContracting(FSTTerminal terminalA,
			FSTTerminal terminalB, FSTTerminal terminalComp) {
		//TODO
		if (terminalA.getBody().contains("also")){
			desugarAlso(terminalA);
		}
		if (terminalB.getBody().contains("also")){
			desugarAlso(terminalB);
		}
		
		List<FSTTerminal> reqClausesA = getRequiresClauses(terminalA);
		List<FSTTerminal> reqClausesB = getRequiresClauses(terminalB);
		List<FSTTerminal> ensClausesA = getEnsuresClauses(terminalA);
		List<FSTTerminal> ensClausesB = getEnsuresClauses(terminalB);
		
		String requiresComp = "";
		String ensuresComp = "";
		String preRequires = "";
		String postRequires = "";
		boolean andOriginal = containsAndOriginal(reqClausesA);
		
		if (!andOriginal){
			preRequires = "!FM.FeatureModel." + getFeatureState(terminalA) + " ==> (";
			postRequires = ");";
		}
		
		if (!modelInfo.isObligatory(getFeatureName(terminalA)) || andOriginal){
			for (FSTTerminal requiresB: reqClausesB){
				requiresComp += "\r\n\trequires " + preRequires + requiresB.getBody() + postRequires + ";";
			}
		}
			
		terminalComp.setBody(getReplacementString(terminalA, terminalB));
	}
	
	private boolean containsAndOriginal(List<FSTTerminal> clauses){
		boolean result = false;
		for (FSTTerminal clause : clauses){
			if (clause.getBody().trim().equals(ORIGINAL_KEYWORD)){
				clause.setBody("");
				result = true;
			}
		}
		return result;
	}

	protected void consecutiveContracting(FSTTerminal terminalA,
			FSTTerminal terminalB, FSTTerminal terminalComp) {
		// remove also from Spezifications
		if (terminalA.getBody().contains("also")){
			desugarAlso(terminalA);
		}
		if (terminalB.getBody().contains("also")){
			desugarAlso(terminalB);
		}
		
		// get all Clauses
		List<FSTTerminal> reqClausesA = getRequiresClauses(terminalA);
		List<FSTTerminal> reqClausesB = getRequiresClauses(terminalB);
		List<FSTTerminal> ensClausesA = getEnsuresClauses(terminalA);
		List<FSTTerminal> ensClausesB = getEnsuresClauses(terminalB);

		// build new Clauses: Requires = \original || (FM.FeatureModel.FeatureName && (new Clauses))
		String terminalARequires = reqClausesA.size() > 0 ? joinClause(reqClausesA,"requires") : "";
		String terminalBRequires = reqClausesB.size() > 0 ? joinClause(reqClausesB,"requires") : "";
		String terminalCompBody = "";
		String featureStateA = getFeatureState(terminalA);
		String preRequires = "";
		String postRequires = "";
		String preEnsures = "";
		String postEnsures = "";
		
		if (!terminalARequires.trim().isEmpty()){
			preEnsures = terminalARequires + " ==> (";
			postEnsures =")";
		}
		
		if (!modelInfo.isObligatory(getFeatureName(terminalA))){
			preRequires = "FM.FeatureModel." + featureStateA + " && (";
			postRequires = ")";
			if (preEnsures.isEmpty()){ // no requires
				preEnsures = "FM.FeatureModel." + featureStateA + " ==> (";
			} else {
				preEnsures = "(FM.FeatureModel." + featureStateA + " && \\old" + terminalARequires + ") ==> (";
			}
			postEnsures = ")";
		}
		
		if (!terminalARequires.trim().isEmpty() && !terminalBRequires.trim().isEmpty()){
			terminalCompBody = "requires " + terminalBRequires + " || (" + preRequires + terminalARequires + postRequires + ");";
		} else if (!terminalARequires.trim().isEmpty()){
			terminalCompBody = "requires " + preRequires + terminalARequires + postRequires + ";";
		} else if (!terminalBRequires.trim().isEmpty()) {
			terminalCompBody = "requires " + terminalBRequires + ";";
		}
		
		for (FSTTerminal ensuresB : ensClausesB){
			terminalCompBody += "\r\n\tensures " + ensuresB.getBody().replaceAll("ensures ","").replaceAll(";","") + ";";
		}
		
		for (FSTTerminal ensuresA : ensClausesA){
			terminalCompBody += "\r\n\tensures "+ preEnsures + ensuresA.getBody().replaceAll("ensures ","").replaceAll(";","") + postEnsures + ";";
		}
		
		terminalComp.setBody(terminalCompBody);
		
	}

	protected void conjunctiveContracting(FSTTerminal terminalA,
			FSTTerminal terminalB, FSTTerminal terminalComp) {
		if (terminalB.getBody().contains("also"))
			desugarAlso(terminalB);
		if (terminalA.getBody().contains("also"))
			desugarAlso(terminalA);
		
		List<FSTTerminal> reqClaB = getRequiresClauses(terminalB);
		List<FSTTerminal> reqClaA = getRequiresClauses(terminalA);
		List<FSTTerminal> ensClaB = getEnsuresClauses(terminalB);
		List<FSTTerminal> ensClaA = getEnsuresClauses(terminalA);
		String terminalCompBody = "";
		String preClause = "";
		String postClause = "";
		
		if (!modelInfo.isObligatory(getFeatureName(terminalA))) {
			preClause = "FM.FeatureModel." + getFeatureState(terminalA) + " ==> (";
			postClause = ")";
		}
				
		for (FSTTerminal requiresB : reqClaB){
			terminalCompBody += "\r\n\trequires " + requiresB.getBody().replaceAll("requires ","").replaceAll(";","") + ";"; 
		}
		
		for (FSTTerminal requiresA : reqClaA){
			terminalCompBody += "\r\n\trequires " + preClause + requiresA.getBody().replaceAll("requires ","").replaceAll(";","") + postClause + ";";
		}
		
		for (FSTTerminal ensuresB : ensClaB){
			terminalCompBody += "\r\n\tensures " + ensuresB.getBody().replaceAll("ensures ","").replaceAll(";","") + ";"; 
		}
		
		for (FSTTerminal ensuresA : ensClaA){
			terminalCompBody += "\r\n\tensures " + preClause + ensuresA.getBody().replaceAll("ensures ","").replaceAll(";","") + postClause + ";";
		}
		
		terminalComp.setBody(terminalCompBody);
		
	}

	protected void cumulativeContracting(FSTTerminal terminalA,
			FSTTerminal terminalB, FSTTerminal terminalComp) {
		if (terminalB.getBody().contains("also"))
			desugarAlso(terminalB);
		if (terminalA.getBody().contains("also"))
			desugarAlso(terminalA);

		List<FSTTerminal> reqClaB = getRequiresClauses(terminalB);
		List<FSTTerminal> reqClaA = getRequiresClauses(terminalA);
		List<FSTTerminal> ensClaB = getEnsuresClauses(terminalB);
		List<FSTTerminal> ensClaA = getEnsuresClauses(terminalA);

		String terminalCompBody = "";
		String requiresA = reqClaA.size() > 0 ? joinClause(reqClaA,"requires") : "";
		String requiresB = reqClaB.size() > 0 ? joinClause(reqClaB,"requires") : "";
		
		String preRequires = "";
		String postRequires = "";
		String preEnsures = "";
		String postEnsures = "";
		String featureState = getFeatureState(terminalA);
		
		if (!modelInfo.isObligatory(getFeatureName(terminalA))){
			preRequires = "FM.FeatureModel." + featureState + " && (";
			postRequires = ")";
			preEnsures = "FM.FeatureModel." + featureState + " ==> (";
			postEnsures = ")";
		}
		
		if (!requiresA.trim().isEmpty() && !requiresB.trim().isEmpty()){
			terminalCompBody = "requires (" + requiresB + ")\r\n\t\t || (" + preRequires + requiresA + postRequires + ");";
		} else if (!requiresA.trim().isEmpty()){
			terminalCompBody = "requires " + preRequires + requiresA + postRequires + ";";
		} else if (!requiresB.trim().isEmpty()) {
			terminalCompBody = "requires " + requiresB + ");";
		}
		
		for (FSTTerminal ensuresB : ensClaB){
			terminalCompBody += "\r\n\tensures " + ensuresB.getBody().replaceAll("ensures ","").replaceAll(";","") + ";";
		}
		
		for (FSTTerminal ensuresA : ensClaA){
			terminalCompBody += "\r\n\tensures " + preEnsures + ensuresA.getBody().replaceAll("ensures ","").replaceAll(";","") + postEnsures + ";";
		}
		
		terminalComp.setBody(terminalCompBody);
	}

	@Override
	protected List<FSTTerminal> getRequiresClauses(FSTTerminal terminal){
		if (terminal.getBody().trim().isEmpty())
			return new ArrayList<FSTTerminal>();
		return super.getRequiresClauses(terminal);
	}

	@Override
	protected List<FSTTerminal> getEnsuresClauses(FSTTerminal terminal){
		if (terminal.getBody().trim().isEmpty())
			return new ArrayList<FSTTerminal>();
		return super.getEnsuresClauses(terminal);
	}

}