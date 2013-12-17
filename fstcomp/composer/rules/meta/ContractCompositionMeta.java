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

	private static final String REQUIRE_OR_ORIGINAL = "FM.Features.OrOriginal";
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
		state = state.replaceAll("()","");
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
		
		if (terminalB.getBody().contains("\\not_composed\r\n")){
			FSTTerminal newComp = (FSTTerminal)terminalComp.getDeepClone();
			FSTTerminal newB = (FSTTerminal)terminalB.getDeepClone();
			newComp.setParent(terminalComp.getParent());
			newB.setParent(terminalB.getParent());
			newB.setBody("\r\n\trequires " + REQUIRE_OR_ORIGINAL + ";");
			compose(terminalB,newB,newComp,nonterminalParent);
			terminalB = newComp;
		}

		String body = terminalA.getBody();
		body = body .replaceAll("\\\\not_composed\r\n","");		
		terminalA.setBody(removeRequireOrOriginal(body));
		super.compose(terminalA,terminalB,terminalComp,nonterminalParent);
		terminalA.setBody(body);
	}
	
	
	/**
	 * TODO description
	 */
	@Override
	public void preCompose(FSTTerminal terminal) {
		// TODO
		
		String body = terminal.getBody();
		terminal.setBody(
				  "\\not_composed\r\n\trequires FM.FeatureModel." 
				+ getFeatureState(terminal) 
				+ REQUIRE_OR_ORIGINAL + ";\r\n\t" 
				+ body);
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
		body = body.replaceAll(REQUIRE_OR_ORIGINAL, "");
		body = body.replaceAll("requires  || ", "");
		body = body.replaceAll("\\" + ORIGINAL_KEYWORD, "true");
		body = body.replaceAll("\\\\not_composed", "");
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
	
	private String getNewReqOrOriginal(String oldRequiresClause, String featureState){
		String clauseBody = oldRequiresClause.replaceAll(REQUIRE_OR_ORIGINAL, " || FM.FeatureModel." + featureState + REQUIRE_OR_ORIGINAL);
		clauseBody = clauseBody.replace("requires  || ", "requires ");
		
		return clauseBody;
	}
	
	private String removeRequireOrOriginal(String body){
		return body.replaceAll("requires FM.FeatureModel.[\\w]+" + REQUIRE_OR_ORIGINAL + ";","");
	}
	
	protected void compositionByKeywords(FSTTerminal terminalA,
			FSTTerminal terminalB, FSTTerminal terminalComp,
			FSTNonTerminal nonterminalParent) {
		// TODO
		String compositionKey = "";
		FSTNode parent = terminalB.getParent();
		List<FSTNode> children = ((FSTNonTerminal)parent).getChildren();

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

		List<FSTTerminal> reqClaB = getRequiresClauses(terminalB);
		
		if (modelInfo.isObligatoryForMethod(getMethodName(terminalA),featureNameA)){
			// overrides in every product
			String compBody = "";
			for (FSTTerminal clause : reqClaB){
				String clauseBody = clause.getBody();
				if (clauseBody.contains(REQUIRE_OR_ORIGINAL)){
					compBody = getNewReqOrOriginal(clauseBody,featureStateA);
					break;
				}
			}
			compBody += "\r\n\t" + terminalA.getBody();			
			terminalComp.setBody(compBody);
		} else {
			// convert also, otherwise resulting contracts would not be correct
			if (terminalA.getBody().contains("also")){
				desugarAlso(terminalA);
			}
			if (terminalB.getBody().contains("also")){
				desugarAlso(terminalB);
			}
			// overrides only if Feature is selected			
			List<FSTTerminal> reqClaA = getRequiresClauses(terminalA);
			List<FSTTerminal> ensClaB = getEnsuresClauses(terminalB);
			List<FSTTerminal> ensClaA = getEnsuresClauses(terminalA);
			
			String terminalCompBody = "";
			
			// old requires-clauses
			for (FSTTerminal requiresB : reqClaB){
				
				String requiresBody = requiresB.getBody();
				
				if (requiresBody.contains(REQUIRE_OR_ORIGINAL)){
					terminalCompBody += getNewReqOrOriginal(requiresBody, featureNameA);
					continue;
				}
				
				List<String> selected = getSelectedFeatures(requiresBody);
				List<String> rejected = getRejectedFeatures(requiresBody);
				
				rejected.add(featureNameA);				
				if (modelInfo.hasValidProduct(selected, rejected)){
					// only add if combination could be possible (allredy selected and rejected Features plus reject Feature of TerminalB can lead to a valid Product)
					terminalCompBody += "\r\n\t requires !FM.FeatureModel." + featureStateA + " ==> (" + requiresBody + ");";
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
		
		String featureState = getFeatureState(terminalA);
		String methodName = getMethodName(terminalA);
		boolean isObligatory = modelInfo.isObligatoryForMethod(methodName,getFeatureName(terminalA));
		
		String bodyComp = "";
		
		for (FSTTerminal clause : reqClausesB){
			String clauseBody = clause.getBody();
			if (clauseBody.contains(REQUIRE_OR_ORIGINAL)){
				clause.setBody("");
				bodyComp += getNewReqOrOriginal(clauseBody, featureState);
				break;
			}
		}
		
		bodyComp += explicitComposeClauses(reqClausesB, reqClausesA, 
				terminalB, "requires", featureState, isObligatory);
		
		bodyComp += explicitComposeClauses(ensClausesB, ensClausesA, 
				terminalB, "ensures", featureState, isObligatory);
		
		
			
		terminalComp.setBody(bodyComp);
		//terminalComp.setBody(getReplacementString(terminalA, terminalB));
	}
	
	private String getMethodName(FSTNode node){
		if (node == null)
			return "";
		if (node.getType().equals("MethodDeclarationWithSpec")){
			String name = node.getName();
			if (name.contains("(")){
				name = name.substring(0,name.indexOf("(")).trim();
			}
			return name; 
		}
		return getMethodName(node.getParent());
	}
	
	private boolean removeAndOriginal(List<FSTTerminal> clauses, String clauseType){
		// TODO 
		boolean result = false;
		for (FSTTerminal clause : clauses){
			if (clause.getBody().trim().equals(clauseType + " " + ORIGINAL_KEYWORD + ";")){
				clause.setBody("");
				result = true;
			}
		}
		return result;
	}
		
	private String simplifyImplications(String clause){
		String simplifiedClause = clause;
		ArrayList<String> selectedFeatures = new ArrayList<String>();
		ArrayList<String> rejectedFeatures = new ArrayList<String>();
		Pattern pat = Pattern.compile("FM.FeatureModel.([\\w]*(\\(\\))?) ==> ");
		Matcher match = pat.matcher(clause);
		while (match.find()){
			String featureName = stateToFeatureName(match.group(1));
			char prefChar = match.start() > 0 ? clause.charAt(match.start()-1) : ' ';
			boolean implied = (prefChar != '!') ?
								modelInfo.isFeatureImplied(featureName,selectedFeatures,rejectedFeatures) :
								modelInfo.isNotFeatureImplied(featureName,selectedFeatures,rejectedFeatures);
			if (implied){
				simplifiedClause = simplifiedClause.replaceAll("!?FM.FeatureModel." + match.group(1).replaceAll("\\(\\)","") + "\\(?\\)? ==> ", "");
			} else {
				if (prefChar == '!')
					rejectedFeatures.add(featureName);
				else
					selectedFeatures.add(featureName);
			}
			
		}
		
		return simplifiedClause;
	}
	
	private String explicitComposeClauses(List<FSTTerminal> originalClauses, List<FSTTerminal> newClauses, 
			FSTTerminal originalTerminal, String type, String featureState, boolean isObligatory){

		String result = "";
		String pre = "";
		String post = "";
		boolean andOriginal = removeAndOriginal(newClauses, type);
		// Implikation nur, wenn Spezifikation nicht erweitert (nicht "requires \original"; oder vergleichbare Klausel enthält
		if (!andOriginal){
			pre = "!FM.FeatureModel." + featureState + " ==> (";
			post = ")";
			
		}
		
		// Originale Klauseln überspringen, wenn FeatureA obligatoisch ist
		// ausnahme: FeatureA ist zwar obligatorisch, erweitert aber die Spezifikation
		if (!isObligatory || andOriginal) {
			for (FSTTerminal clause : originalClauses){
				if (clause.getBody().trim().isEmpty())
					continue;
				String newClause = "\r\n\t"
								+ type
								+ " "
								+ pre
								+ clause.getBody().replace(type + " ", "").replaceAll(";", "")
								+ post
								+ ";";
				// Klausel nicht hinzufügen, falls Featurekombination nicht möglich ist
				List<String> selected = getSelectedFeatures(newClause);
				List<String> rejected = getRejectedFeatures(newClause);
				if (modelInfo.hasValidProduct(selected, rejected))
					result += simplifyImplications(newClause);
			}
		}
		
		// neue Klauseln immer hinzufügen.
		// wenn nicht obligatorisch, dann inkl. Implikation
		pre = "";
		post = "";
		if (!isObligatory){
			pre = "FM.FeatureModel."
					  + featureState
					  + " ==> (";
			post = ")";
		}
		for (FSTTerminal clause : newClauses){
			if (clause.getBody().trim().isEmpty())
				continue;
			result += "\r\n\t"
					+ type
					+ " "
					+ pre;
			
			String clauseBody = clause.getBody();
			
			if (clauseBody.contains(ORIGINAL_KEYWORD)
					|| clauseBody.contains(ORIGINAL_KEYWORD_CLAUSE)
					|| clauseBody.contains(ORIGINAL_CASE_KEYWORD)
					|| clauseBody.contains(ORIGINAL_SPEC_KEYWORD)){
				result += replaceOriginal(
							new String[] {
								originalTerminal.getBody()
									.replaceAll("requires (FM.FeatureModel.[\\w]+\\(?\\)?)?( || FM.FeatureModel.[\\w]+\\(?\\)?)*" 
												+ REQUIRE_OR_ORIGINAL + ";", "")}
							, clauseBody
							, 0
							, type
							).replace(type + " ", "").replaceAll(";","");
			} else {
				result += clauseBody.replace(type + " ", "").replaceAll(";","");
			}
			
			result += post + ";";
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
		String terminalCompBody = "";
		String featureStateA = getFeatureState(terminalA);
		String preRequires = "";
		String postRequires = "";
		String preEnsures = "";
		String postEnsures = "";
		
		// get all Clauses
		List<FSTTerminal> reqClausesA = getRequiresClauses(terminalA);
		List<FSTTerminal> reqClausesB = getRequiresClauses(terminalB);
		List<FSTTerminal> ensClausesA = getEnsuresClauses(terminalA);
		List<FSTTerminal> ensClausesB = getEnsuresClauses(terminalB);
		
		// Search for the REquires or original-Clause
		for (FSTTerminal clause : reqClausesB){
			String clauseBody = clause.getBody();
			if (clauseBody.contains(REQUIRE_OR_ORIGINAL)){
				terminalCompBody = getNewReqOrOriginal(clauseBody, featureStateA);
				reqClausesB.remove(clause);
				break;
			}
		}

		// build new Clauses: Requires = \original || (FM.FeatureModel.FeatureName && (new Clauses))
		String terminalARequires = reqClausesA.size() > 0 ? joinClause(reqClausesA,"requires") : "";
		String terminalBRequires = reqClausesB.size() > 0 ? joinClause(reqClausesB,"requires") : "";
		
		if (!terminalARequires.trim().isEmpty()){
			preEnsures = terminalARequires + " ==> (";
			postEnsures =")";
		}
		
		if (!modelInfo.isObligatoryForMethod(getMethodName(terminalA),getFeatureName(terminalA))){
			preRequires = "(FM.FeatureModel." + featureStateA + " && ";
			postRequires = ")";
			if (preEnsures.isEmpty()){ // no requires
				preEnsures = "FM.FeatureModel." + featureStateA + " ==> (";
			} else {
				preEnsures = "(FM.FeatureModel." + featureStateA + " && \\old" + terminalARequires + ") ==> (";
			}
			postEnsures = ")";
		}
		
		if (!terminalARequires.trim().isEmpty() && !terminalBRequires.trim().isEmpty()){
			terminalCompBody += "\r\n\trequires " + terminalBRequires + "\r\n\t\t || " + preRequires + terminalARequires + postRequires + ";";
		} else if (!terminalARequires.trim().isEmpty()){
			terminalCompBody += "\r\n\trequires " + preRequires + terminalARequires + postRequires + ";";
		} else if (!terminalBRequires.trim().isEmpty()) {
			terminalCompBody += "\r\n\trequires " + terminalBRequires + ";";
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
		String featureState = getFeatureState(terminalA);
		
		if (!modelInfo.isObligatoryForMethod(getMethodName(terminalA),getFeatureName(terminalA))) {
			preClause = "FM.FeatureModel." + featureState + " ==> (";
			postClause = ")";
		}
				
		for (FSTTerminal requiresB : reqClaB){
			String clauseBody = requiresB.getBody();
			if (clauseBody.contains(REQUIRE_OR_ORIGINAL)){
				terminalCompBody += getNewReqOrOriginal(clauseBody, featureState);
				continue;
			}
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
		String preRequires = "";
		String postRequires = "";
		String preEnsures = "";
		String postEnsures = "";
		String featureState = getFeatureState(terminalA);
		
		// Search for the REquires or original-Clause
		for (FSTTerminal clause : reqClaB){
			String clauseBody = clause.getBody();
			if (clauseBody.contains(REQUIRE_OR_ORIGINAL)){
				terminalCompBody = getNewReqOrOriginal(clauseBody, featureState);
				reqClaB.remove(clause);
				break;
			}
		}
		
		
		String requiresA = reqClaA.size() > 0 ? joinClause(reqClaA,"requires") : "";
		String requiresB = reqClaB.size() > 0 ? joinClause(reqClaB,"requires") : "";
		
		if (!modelInfo.isObligatoryForMethod(getMethodName(terminalA),getFeatureName(terminalA))){
			preRequires = "FM.FeatureModel." + featureState + " && ";
			postRequires = "";
			preEnsures = "FM.FeatureModel." + featureState + " ==> (";
			postEnsures = ")";
		}
		
		if (!requiresA.trim().isEmpty() && !requiresB.trim().isEmpty()){
			terminalCompBody += "\r\n\trequires " + requiresB + "\r\n\t\t || (" + preRequires + requiresA + postRequires + ");";
		} else if (!requiresA.trim().isEmpty()){
			terminalCompBody += "\r\n\trequires " + preRequires + requiresA + postRequires + ";";
		} else if (!requiresB.trim().isEmpty()) {
			terminalCompBody += "\r\n\trequires " + requiresB + ");";
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
		if (terminal.getBody().replaceAll("\r","").replaceAll("\n","").replaceAll("\t","").trim().isEmpty())
			return new ArrayList<FSTTerminal>();
		return super.getRequiresClauses(terminal);
	}

	@Override
	protected List<FSTTerminal> getEnsuresClauses(FSTTerminal terminal){
		if (terminal.getBody().replaceAll("\r","").replaceAll("\n","").replaceAll("\t","").trim().isEmpty())
			return new ArrayList<FSTTerminal>();
		return super.getEnsuresClauses(terminal);
	}

}