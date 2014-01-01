package composer.rules.meta;



import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
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
	private static final String FINAL_CONTRACT = "requires FM.Contract.finalContract;";
	private static final String COMPOSITION_EXPLICIT = "FM.CompositionExplicit";
	private static final String COMPOSITION_CONJUNCTIVE = "FM.CompositionConjunctive";
	private static final String COMPOSITION_CONSECUTIVE = "FM.CompositionConsecutive";
	private static final String COMPOSITION_CUMULATIVE = "FM.CompositionCumulative";
	private static final String COMPOSITION_PLAIN = "FM.CompositionPlain";
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

	//private List<String> getSelectedFeatures(String clause){
	private void selectFeaturesFromClause(String clause){
		Pattern featurePattern = Pattern.compile("[^!]FM\\.FeatureModel\\.([\\w]+)" + (FSTGenComposerExtension.key ? "" : "\\(\\)") +" ==>");
		Matcher featureMatcher = featurePattern.matcher(" " + clause);
		//List<String> featureList = new ArrayList<String>();
		while (featureMatcher.find()){
			modelInfo.selectFeature(stateToFeatureName(featureMatcher.group(1)));
			//featureList.add(stateToFeatureName(featureMatcher.group(1)));
		}
		//return featureList;
	}
	
	//private List<String> getRejectedFeatures(String clause){
	private void rejectFeaturesFromClause(String clause){
		Pattern featurePattern = Pattern.compile("!FM\\.FeatureModel\\.([\\w]+)" + (FSTGenComposerExtension.key ? "" : "\\(\\)") +" ==>");
		Matcher featureMatcher = featurePattern.matcher(clause);
		//List<String> featureList = new ArrayList<String>();
		while (featureMatcher.find()){
			modelInfo.rejectFeature(stateToFeatureName(featureMatcher.group(1)));
			//featureList.add(stateToFeatureName(featureMatcher.group(1)));
		}
		//return featureList;
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
	
	private String getNewReqOrOriginal(String oldRequiresClause, String featureState){
		String clauseBody = oldRequiresClause.replaceAll(REQUIRE_OR_ORIGINAL, "FM.FeatureModel." + featureState + " || " + REQUIRE_OR_ORIGINAL);
		//clauseBody = clauseBody.replace("requires  || ", "requires ");
		
		return clauseBody + ";";
	}
	
	private String removeRequireOrOriginal(String body){
		return body.replaceAll("requires FM.FeatureModel.[\\w]+" + (FSTGenComposerExtension.key ? "" : "\\(\\)") +" \\|\\| " + REQUIRE_OR_ORIGINAL + ";","");
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
	
	private String getClassName(FSTNode node){
		if (node == null)
			return "";
		if (node.getType().contains("ClassDeclaration"))
			return node.getName();
		return getClassName(node.getParent());
	}
	
	private boolean removeAndOriginal(List<FSTTerminal> clauses, String clauseType){
		// TODO 
		boolean result = false;
		for (FSTTerminal clause : clauses){
			String body = clause.getBody();
			// skip the rest if /original is not in the clause
			if (!body.contains(ORIGINAL_KEYWORD))
				continue;
			
			// find out, whether it is composend with an and:
			// Possibilities:
			//	1. requires \original;
			// 	2. requires \original && ...
			while(body.contains("  ")){
				body = body.replaceAll("  "," ");
			}
			
			// 1.
			if (body.trim().equals(clauseType + " " + ORIGINAL_KEYWORD)){
				clause.setBody("");
				result = true;
			}
			
			// 2. (first delete all linefeeds, double spaces, tabs)
			body = body.replaceAll("\r", "")
						.replaceAll("\n", "")
						.replaceAll("\t", "");
			while(body.contains("  ")){
				body = body.replaceAll("  "," ");
			}
			
			Pattern p = Pattern.compile(clauseType + "[\\s]+\\" + ORIGINAL_KEYWORD + "[\\s]+&&[\\s]+");
			Matcher m = p.matcher(body);
			if (m.find()){
				clause.setBody(clause.getBody().replaceAll(p.pattern(),clauseType + " "));
				result = true;
			}
		}
		return result;
	}
	
	private String simplifyImplications(String clause){
		String simplifiedClause = clause;
		Pattern pat = Pattern.compile("FM\\.FeatureModel\\.([\\w]+)" + (FSTGenComposerExtension.key ? "" : "\\(\\)") +" ==>");
		Matcher match = pat.matcher(clause);
		modelInfo.reset();
		while (match.find()){
			String featureName = stateToFeatureName(match.group(1));
			char prefChar = match.start() > 0 ? clause.charAt(match.start()-1) : ' ';
			if (prefChar == '!'){
				if (modelInfo.isRejectionImplied(featureName))
					simplifiedClause = simplifiedClause.replaceAll("!FM.FeatureModel." + match.group(1).replaceAll("\\(\\)","") + "\\(?\\)? ==> ", "");
				else
					modelInfo.rejectFeature(featureName);
			} else {
				if (modelInfo.isSelectionImplied(featureName))
					simplifiedClause = simplifiedClause.replaceAll("([^!])FM.FeatureModel." + match.group(1).replaceAll("\\(\\)","") + "\\(?\\)? ==> ", "$1");
				else
					modelInfo.selectFeature(featureName);
			}			
		}
		
		return simplifiedClause;
	}
	
	private Set<String> getFeatures(List<FSTTerminal> clauses, String clauseType){
		HashSet<String> result = new HashSet<String>();
		
		Pattern p = Pattern.compile(clauseType + " !?FM\\.FeatureModel\\.([\\w]+" + (FSTGenComposerExtension.key ? "" : "\\(\\)") +") ==> \\(");
		
		for (FSTTerminal clause : clauses){
			String clauseBody = clause.getBody();
			Matcher m = p.matcher(clauseBody);
			while (m.find()){
				result.add(m.group(1));
				clauseBody = clauseBody.replaceAll(p.pattern(), clauseType + " ");
				m = p.matcher(clauseBody);
			}
		}
		
		return result;
	}
	
	private String getCompositionKey(FSTTerminal terminal){
		String compKey = "";
		for (FSTNode n : ((FSTNonTerminal) terminal.getParent()).getChildren()) {
			if (n.getType().equals("ContractCompKey")) {
				compKey = ((FSTTerminal) n).getContractCompKey();
				break;
			}
		}
		return compKey;
	}
	
	private String[] extractCompositionInformation(String clause,String type){
		String[] result = {"","","",""}; //Composition-Type, Parameters, resulting prefix, resulting suffix
		Pattern p = Pattern.compile(
					type
				+	" (" + COMPOSITION_CONJUNCTIVE + "|" + COMPOSITION_CONSECUTIVE + "|" + COMPOSITION_CUMULATIVE + "|"
						+ COMPOSITION_EXPLICIT + "|" + COMPOSITION_PLAIN + ")\\("
				+	"((!?[\\w]+\\(?\\)?)?(,(!?[\\w]+\\(?\\)?))*)"
				+ 	"\\)"
				);
		Matcher m = p.matcher(clause);
		if (m.find()){
			result[0] = m.group(1);
			result[1] = m.group(2);
			Pattern pf = Pattern.compile(",(!?[\\w]+\\(?\\)?),");
			Matcher mf = pf.matcher(","+result[1]+",");
			while (mf.find()){
				if (mf.group(1).charAt(0) == '!')
					result[2] += "!FM.FeatureModel." + mf.group(1).substring(1) + " ==> (";
				else
					result[2] += "FM.FeatureModel." + mf.group(1) + " ==> (";
				result[3] += ")";
			}
			
		}
			
		return result;
	}
	
	private String[] createCompositionInfo(String compKey, String featureState){
		String[] result = new String[4];
		if (compKey.equals(CompositionKeyword.CONJUNCTIVE_CONTRACT))
			result[0] = COMPOSITION_CONJUNCTIVE;
		else if (compKey.equals(CompositionKeyword.CONSECUTIVE_CONTRACT))
			result[0] = COMPOSITION_CONSECUTIVE;
		else if (compKey.equals(CompositionKeyword.CUMULATIVE_CONTRACT))
			result[0] = COMPOSITION_CUMULATIVE;
		else if (compKey.equals(CompositionKeyword.FINAL_CONTRACT) || compKey.equals(CompositionKeyword.FINAL_METHOD))
			result[0] = COMPOSITION_PLAIN;
		else
			result[0] = COMPOSITION_EXPLICIT;
		result[1] = featureState;
		result[2] = "FM.FEatureMode." + featureState + " ==> (";
		result[3] = ")";
		return result;
	}
	
	private void getNewClauses(FSTTerminal terminalA,FSTTerminal terminalB, FSTTerminal terminalComp,
			String[] compInfo, List<FSTTerminal> clausesB,List<FSTTerminal> resultingRequires,List<FSTTerminal> resultingEnsures, boolean withCompInfoPrefix){
		
		if (clausesB == null)
			return;
		
		//if (clausesB.size() == 0)
		//	return;
		
		if (compInfo == null)
			return;
		
		// first get copies of the Terminals
		FSTTerminal newTerminalA = (FSTTerminal)terminalA.getDeepClone();
		FSTTerminal newTerminalB = (FSTTerminal)terminalA.getDeepClone();
		FSTTerminal newTerminalComp = (FSTTerminal)terminalA.getDeepClone();
		
		newTerminalA.setParent(terminalA.getParent());
		newTerminalB.setParent(terminalB.getParent());
		newTerminalComp.setParent(terminalComp.getParent());
		
		newTerminalB.setBody("");
		newTerminalComp.setBody("");
		
		// fill newTerminalB
		String bodyB = "";
		String pre = withCompInfoPrefix ? compInfo[2] : "";
		String post = withCompInfoPrefix ? compInfo[3] : ""; 
		
		for (FSTTerminal clause : clausesB){
			if (clause.getBody().contains("requires"))
				bodyB += "requires " + pre + clause.getBody().replaceAll("requires ", "") + post + ";"; 
			else if (clause.getBody().contains("ensures"))
				bodyB += "ensures " + pre + clause.getBody().replaceAll("ensures ", "") + post + ";";
		}
		newTerminalB.setBody(bodyB);
		
		if (compInfo[0].equals(COMPOSITION_CONJUNCTIVE))
			conjunctiveContracting(newTerminalA,newTerminalB,newTerminalComp);
		else if (compInfo[0].equals(COMPOSITION_CONSECUTIVE))
			consecutiveContracting(newTerminalA, newTerminalB, newTerminalComp);
		else if (compInfo[0].equals(COMPOSITION_CUMULATIVE))
			cumulativeContracting(newTerminalA, newTerminalB, newTerminalComp);
		else if (compInfo[0].equals(COMPOSITION_PLAIN))
			plainContracting(newTerminalA, newTerminalB, newTerminalComp);
		else
			explicitContracting(newTerminalA, newTerminalB, newTerminalComp);
		
		resultingRequires.addAll(getRequiresClauses(newTerminalComp));
		resultingEnsures.addAll(getEnsuresClauses(newTerminalComp));
		
	}
	
	private String getCompInfoForTerminal(FSTTerminal terminalB){
		if (!contractStyle.equals(METHOD_BASED_COMPOSITION))
			return "";
		String compKey = getCompositionKey(terminalB);
		if (compKey.equals(CompositionKeyword.CONJUNCTIVE_CONTRACT.getLabel()))
			return "\r\n\trequires " + COMPOSITION_CONJUNCTIVE + "();";
		if (compKey.equals(CompositionKeyword.CONSECUTIVE_CONTRACT.getLabel()))
			return "\r\n\trequires " + COMPOSITION_CONSECUTIVE + "();";
		if (compKey.equals(CompositionKeyword.CUMULATIVE_CONTRACT.getLabel()))
			return "\r\n\trequires " + COMPOSITION_CUMULATIVE + "();";
		if (compKey.equals(CompositionKeyword.FINAL_CONTRACT.getLabel())|| compKey.equals(CompositionKeyword.FINAL_METHOD.getLabel()))
			return "\r\n\trequires " + COMPOSITION_PLAIN + "();";
		return "\r\n\trequires " + COMPOSITION_EXPLICIT + "();";
	}
	
	private void setSelectedRejectedFromCompInfo(MethodBasedModelInfoWrapper specialModelInfo,String[] compInfo){
		specialModelInfo.clear();
		if (compInfo == null)
			return;
		
		Pattern pf = Pattern.compile(",(!?[\\w]+\\(?\\)?),");
		Matcher mf = pf.matcher(","+compInfo[1]+",");
		while (mf.find()){
			if (mf.group(1).charAt(0) == '!')
				specialModelInfo.setRejected(stateToFeatureName(mf.group(1).substring(1)));
			else
				specialModelInfo.setSelected(stateToFeatureName(mf.group(1)));
		}
		
	}
	
	@Override
	@Deprecated
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

	@Override
	public void compose(FSTTerminal terminalA, FSTTerminal terminalB,
			FSTTerminal terminalComp, FSTNonTerminal nonterminalParent)
			throws CompositionException {
		
		if (terminalB.getBody().contains("\\not_composed\r\n")){
			FSTTerminal newComp = (FSTTerminal)terminalB.getDeepClone();
			FSTTerminal newB = (FSTTerminal)terminalB.getDeepClone();
			newComp.setParent(terminalB.getParent());
			newB.setParent(terminalB.getParent());
			newB.setBody("\r\n\trequires " + REQUIRE_OR_ORIGINAL + ";"
					+ getCompInfoForTerminal(terminalB)
					);
			compose(terminalB,newB,newComp,nonterminalParent);
			terminalB = newComp;
		}

		String body = terminalA.getBody();
		body = body .replaceAll("\\\\not_composed\r\n","");		
		terminalA.setBody(removeRequireOrOriginal(body));
		super.compose(terminalA,terminalB,terminalComp,nonterminalParent);
		terminalA.setBody(body);
	}
	
	@Override
	public void preCompose(FSTTerminal terminal) {
		// TODO
		
		String body = terminal.getBody();
		terminal.setBody(
				  "\\not_composed\r\n\trequires FM.FeatureModel." 
				+ getFeatureState(terminal) 
				+ " || " + REQUIRE_OR_ORIGINAL + ";\r\n\t" 
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
		body = body.replaceAll(" \\|\\| " + REQUIRE_OR_ORIGINAL, "");
		body = body.replaceAll(FINAL_CONTRACT, "");
		body = body.replaceAll("requires  \\|\\| ", "");
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
		
		if (contractStyle.equals(METHOD_BASED_COMPOSITION)){
			postComposeMethodBased(terminal);
		}
	}
	
	private void postComposeMethodBased(FSTTerminal terminal){
		List<FSTTerminal> reqClauses = getRequiresClauses(terminal);
		List<FSTTerminal> ensClauses = getEnsuresClauses(terminal);
		
		FeatureModelInfo originalModelInfo = modelInfo;
		modelInfo = new MethodBasedModelInfoWrapper(modelInfo);
		
		String[] currentCompInfo = {"","","",""};
		String newBody = "";
		
		for (FSTTerminal clause : reqClauses){
			if (clause.getBody().contains(REQUIRE_OR_ORIGINAL)){
				newBody += "\r\n\t" + clause.getBody() + ";";
				continue;
			}
			if (clause.getBody().contains(COMPOSITION_EXPLICIT)){
				currentCompInfo = extractCompositionInformation(clause.getBody(),"requires");
				continue;
			}
			if (clause.getBody().contains(COMPOSITION_CONJUNCTIVE)){
				currentCompInfo = extractCompositionInformation(clause.getBody(),"requires");
				continue;
			}
			if (clause.getBody().contains(COMPOSITION_CUMULATIVE)){
				currentCompInfo = extractCompositionInformation(clause.getBody(),"requires");
				continue;
			}
			if (clause.getBody().contains(COMPOSITION_CONSECUTIVE)){
				currentCompInfo = extractCompositionInformation(clause.getBody(),"requires");
				continue;
			}
			if (clause.getBody().contains(COMPOSITION_PLAIN)){
				currentCompInfo = extractCompositionInformation(clause.getBody(),"requires");
				continue;
			}
			String newClause = "requires " + currentCompInfo[2] + clause.getBody().replaceAll("requires ","") + currentCompInfo[3] + ";";
			modelInfo.reset();
			selectFeaturesFromClause(newClause);
			rejectFeaturesFromClause(newClause);
			if (modelInfo.isValidSelection())
				newBody += "\r\n\t" + simplifyImplications(newClause);
		}

		currentCompInfo[2] = "";
		currentCompInfo[3] = "";
		for (FSTTerminal clause : ensClauses){
			if (clause.getBody().contains(COMPOSITION_EXPLICIT)){
				currentCompInfo = extractCompositionInformation(clause.getBody(),"ensures");
				continue;
			}
			if (clause.getBody().contains(COMPOSITION_CONJUNCTIVE)){
				currentCompInfo = extractCompositionInformation(clause.getBody(),"ensures");
				continue;
			}
			if (clause.getBody().contains(COMPOSITION_CUMULATIVE)){
				currentCompInfo = extractCompositionInformation(clause.getBody(),"ensures");
				continue;
			}
			if (clause.getBody().contains(COMPOSITION_CONSECUTIVE)){
				currentCompInfo = extractCompositionInformation(clause.getBody(),"ensures");
				continue;
			}
			if (clause.getBody().contains(COMPOSITION_PLAIN)){
				currentCompInfo = extractCompositionInformation(clause.getBody(),"ensures");
				continue;
			}
			String newClause = "ensures " + currentCompInfo[2] + clause.getBody().replaceAll("ensures ","") + currentCompInfo[3] + ";";
			modelInfo.reset();
			selectFeaturesFromClause(newClause);
			rejectFeaturesFromClause(newClause);
			if (modelInfo.isValidSelection())
				newBody += "\r\n\t" + simplifyImplications(newClause);
		}
		modelInfo = originalModelInfo;
		
		terminal.setBody(newBody);
	}
	
	@Override
	protected List<FSTTerminal> getRequiresClauses(FSTTerminal terminal){
		if (terminal.getBody().replaceAll("\r","").replaceAll("\n","").replaceAll("\t","").trim().isEmpty())
			return new ArrayList<FSTTerminal>();
		try{
			List<FSTTerminal> clauses = super.getRequiresClauses(terminal);
			for (FSTTerminal clause : clauses) {
				clause.setBody(clause.getBody().substring(0, clause.getBody().length() - 1));
			}
			return clauses;
		} catch (Exception ex){
			System.out.println("-----------------------------------------------------------------------------------------------------------");
			System.out.println("Failed to load RequiresClauses:");
			System.out.println("Feature:      " + getFeatureName(terminal));
			System.out.println("Class:        " + getClassName(terminal));
			System.out.println("Method:       " + getMethodName(terminal));
			System.out.println("TerminalBody: " + terminal.getBody());
			System.out.println("-----------------------------------------------------------------------------------------------------------");
			return new ArrayList<FSTTerminal>();
		}
	}

	@Override
	protected List<FSTTerminal> getEnsuresClauses(FSTTerminal terminal){
		if (terminal.getBody().replaceAll("\r","").replaceAll("\n","").replaceAll("\t","").trim().isEmpty())
			return new ArrayList<FSTTerminal>();
		List<FSTTerminal> clauses = super.getEnsuresClauses(terminal);
		for (FSTTerminal clause : clauses) {
			clause.setBody(clause.getBody().substring(0, clause.getBody().length() - 1));
		}
		return clauses;
	}
	
	@Override
	protected String joinClause(List<FSTTerminal> clauses, String clauseType) {
		String result = super.joinClause(clauses, clauseType);
		
		return result.substring(0,result.length() - 1);
	}
	
	protected void compositionByKeywords(FSTTerminal terminalA,
			FSTTerminal terminalB, FSTTerminal terminalComp,
			FSTNonTerminal nonterminalParent) {

		String compositionKeyA = getCompositionKey(terminalA);
		//String compositionKeyB = getCompositionKey(terminalB);
				
		if (terminalA.getBody().contains("also"))
			desugarAlso(terminalA);

		List<FSTTerminal> reqClausesB = getRequiresClauses(terminalB);
		List<FSTTerminal> ensClausesB = getEnsuresClauses(terminalB);
		
		String featureName = getFeatureName(terminalA);
		String featureState = getFeatureState(terminalA);
		String compBodyRequires = "";
		String compBodyEnsures = "";
		
		
		String[] explicitCompInfo = null;//{COMPOSITION_EXPLICIT,"","",""}; // fallback
		String[] conjunctiveCompInfo = null;
		String[] consecutiveCompInfo = null;
		String[] cumulativeCompInfo = null;
		String[] plainCompInfo = null;
		
		List<FSTTerminal> explicitClauses = new LinkedList<FSTTerminal>();
		List<FSTTerminal> conjunctiveClauses = new LinkedList<FSTTerminal>();
		List<FSTTerminal> consecutiveClauses = new LinkedList<FSTTerminal>();
		List<FSTTerminal> cumulativeClauses = new LinkedList<FSTTerminal>();
		List<FSTTerminal> plainClauses = new LinkedList<FSTTerminal>();
		
		List<FSTTerminal> currentClauseList = explicitClauses;
		List<List<FSTTerminal>> filledClauseLists = new LinkedList<List<FSTTerminal>>();
		filledClauseLists.add(explicitClauses);
		CompositionKeyword lastRequiresCompType = null;
		CompositionKeyword lastEnsuresCompType = null;
				
		for (FSTTerminal clause : reqClausesB){
			if (clause.getBody().contains(REQUIRE_OR_ORIGINAL)){
				compBodyRequires += getNewReqOrOriginal(clause.getBody(), getFeatureState(terminalA));
				currentClauseList = explicitClauses;
				continue;
			}
			if (clause.getBody().contains(COMPOSITION_EXPLICIT)){
				explicitCompInfo = extractCompositionInformation(clause.getBody(),"requires");
				currentClauseList = conjunctiveClauses;
				lastRequiresCompType = CompositionKeyword.EXPLICIT_CONTRACT;
				continue;
			}
			if (clause.getBody().contains(COMPOSITION_CONJUNCTIVE)){
				conjunctiveCompInfo = extractCompositionInformation(clause.getBody(),"requires");
				currentClauseList = conjunctiveClauses;
				lastRequiresCompType = CompositionKeyword.CONJUNCTIVE_CONTRACT;
				continue;
			}
			if (clause.getBody().contains(COMPOSITION_CUMULATIVE)){
				cumulativeCompInfo = extractCompositionInformation(clause.getBody(),"requires");
				currentClauseList = cumulativeClauses;
				lastRequiresCompType = CompositionKeyword.CUMULATIVE_CONTRACT;
				continue;
			}
			if (clause.getBody().contains(COMPOSITION_CONSECUTIVE)){
				consecutiveCompInfo = extractCompositionInformation(clause.getBody(),"requires");
				currentClauseList = consecutiveClauses;
				lastRequiresCompType = CompositionKeyword.CONSECUTIVE_CONTRACT;
				continue;
			}
			if (clause.getBody().contains(COMPOSITION_PLAIN)){
				plainCompInfo = extractCompositionInformation(clause.getBody(),"requires");
				currentClauseList = plainClauses;
				lastRequiresCompType = CompositionKeyword.FINAL_CONTRACT;
				continue;
			}
			currentClauseList.add(clause);
		}
		
		//currentClauseList = null;// explicitClauses;
		

		for (FSTTerminal clause : ensClausesB){
			if (clause.getBody().contains(COMPOSITION_EXPLICIT)){
				explicitCompInfo = extractCompositionInformation(clause.getBody(),"ensures");
				currentClauseList = explicitClauses;
				lastEnsuresCompType = CompositionKeyword.EXPLICIT_CONTRACT;
				continue;
			}
			if (clause.getBody().contains(COMPOSITION_CONJUNCTIVE)){
				conjunctiveCompInfo = extractCompositionInformation(clause.getBody(),"ensures");
				currentClauseList = conjunctiveClauses;
				lastEnsuresCompType = CompositionKeyword.CONJUNCTIVE_CONTRACT;
				continue;
			}
			if (clause.getBody().contains(COMPOSITION_CUMULATIVE)){
				cumulativeCompInfo = extractCompositionInformation(clause.getBody(),"ensures");
				currentClauseList = cumulativeClauses;
				lastEnsuresCompType = CompositionKeyword.CUMULATIVE_CONTRACT;
				continue;
			}
			if (clause.getBody().contains(COMPOSITION_CONSECUTIVE)){
				consecutiveCompInfo = extractCompositionInformation(clause.getBody(),"ensures");
				currentClauseList = consecutiveClauses;
				lastEnsuresCompType = CompositionKeyword.CONSECUTIVE_CONTRACT;
				continue;
			}
			if (clause.getBody().contains(COMPOSITION_PLAIN)){
				plainCompInfo = extractCompositionInformation(clause.getBody(),"ensures");
				currentClauseList = plainClauses;
				lastEnsuresCompType = CompositionKeyword.FINAL_CONTRACT;
				continue;
			}
			currentClauseList.add(clause);
		}
		
		boolean compMethodChanged = false;
		
		if (lastEnsuresCompType == null && lastRequiresCompType == null)
			compMethodChanged = false; // dann ist irgenwas falsch
		else if (lastEnsuresCompType == null)
			compMethodChanged = (lastRequiresCompType.getLabel() == compositionKeyA);
		else if (lastRequiresCompType == null)
			compMethodChanged = (lastEnsuresCompType.getLabel() == compositionKeyA);
		else if (lastRequiresCompType.getRank() > lastEnsuresCompType.getRank())
			compMethodChanged = (lastEnsuresCompType.getLabel() == compositionKeyA);
		else
			compMethodChanged = (lastRequiresCompType.getLabel() == compositionKeyA);
			
		
		
		boolean isObligatory = modelInfo.isObligatoryForMethod(getClassName(terminalA), getMethodName(terminalA), featureName);
				
		if(compMethodChanged) {
			if (!isObligatory){
				if (explicitCompInfo != null){
					compBodyRequires += "requires " + explicitCompInfo[0] + "(!" + featureState + (explicitCompInfo[1].isEmpty()?"":"," + explicitCompInfo[1]) + ");";
					compBodyEnsures  += "ensures "  + explicitCompInfo[0] + "(!" + featureState + (explicitCompInfo[1].isEmpty()?"":"," + explicitCompInfo[1]) + ");";
					for (FSTTerminal clause : explicitClauses){
						if (clause.getBody().contains("requires"))
							compBodyRequires += "\r\n\t" + clause.getBody() + ";";
						else if (clause.getBody().contains("ensures"))
							compBodyEnsures += "\r\n\t" + clause.getBody() + ";";
					}
				}
				if (conjunctiveCompInfo != null){
					compBodyRequires += "requires " + conjunctiveCompInfo[0] + "(!" + featureState + (conjunctiveCompInfo[1].isEmpty()?"":"," + conjunctiveCompInfo[1]) + ");";
					compBodyEnsures += "ensures " + conjunctiveCompInfo[0] + "(!" + featureState + (conjunctiveCompInfo[1].isEmpty()?"":"," + conjunctiveCompInfo[1]) + ");";
					for (FSTTerminal clause : conjunctiveClauses){
						if (clause.getBody().contains("requires"))
							compBodyRequires += "\r\n\t" + clause.getBody() + ";";
						else if (clause.getBody().contains("ensures"))
							compBodyEnsures += "\r\n\t" + clause.getBody() + ";";
					}
				}
				if (consecutiveCompInfo != null){
					compBodyRequires += "requires " + consecutiveCompInfo[0] + "(!" + featureState + (consecutiveCompInfo[1].isEmpty()?"":"," + consecutiveCompInfo[1]) + ");";
					compBodyEnsures += "ensures " + consecutiveCompInfo[0] + "(!" + featureState + "," + consecutiveCompInfo[1] + ");";
					for (FSTTerminal clause : consecutiveClauses){
						if (clause.getBody().contains("requires"))
							compBodyRequires += "\r\n\t" + clause.getBody() + ";";
						else if (clause.getBody().contains("ensures"))
							compBodyEnsures += "\r\n\t" + clause.getBody() + ";";
					}
				}
				if (cumulativeCompInfo != null){
					compBodyRequires += "requires " + cumulativeCompInfo[0] + "(!" + featureState + (cumulativeCompInfo[1].isEmpty()?"":"," + cumulativeCompInfo[1]) + ");";
					compBodyEnsures += "ensures " + cumulativeCompInfo[0] + "(!" + featureState + "," + cumulativeCompInfo[1] + ");";
					for (FSTTerminal clause : cumulativeClauses){
						if (clause.getBody().contains("requires"))
							compBodyRequires += "\r\n\t" + clause.getBody() + ";";
						else if (clause.getBody().contains("ensures"))
							compBodyEnsures += "\r\n\t" + clause.getBody() + ";";
					}
				}
				if (plainCompInfo != null){ // hier dürften wir gar nicht rein kommen: wenns schon plain gibt, kann das Verfahren nicht mehr geändert werden
					compBodyRequires += "requires " + plainCompInfo[0] + "(!" + featureState + (plainCompInfo[1].isEmpty()?"":"," + plainCompInfo[1]) + ");";
					compBodyEnsures += "ensures " + plainCompInfo[0] + "(!" + featureState + "," + plainCompInfo[1] + ");";
					for (FSTTerminal clause : plainClauses){
						if (clause.getBody().contains("requires"))
							compBodyRequires += "\r\n\t" + clause.getBody() + ";";
						else if (clause.getBody().contains("ensures"))
							compBodyEnsures += "\r\n\t" + clause.getBody() + ";";
					}
				}
			} // if (!isObligatory) { ...
			
			String[] newCompInfo = createCompositionInfo(compositionKeyA,featureState);
			compBodyRequires += "requires " + newCompInfo[0] + "(" + newCompInfo[1] + ");";
			compBodyEnsures += "ensures " + newCompInfo[0] + "(" + newCompInfo[1] + ");";

			List<FSTTerminal> newRequires = new LinkedList<FSTTerminal>();
			List<FSTTerminal> newEnsures  = new LinkedList<FSTTerminal>();
			
			FeatureModelInfo originalModelInfo = modelInfo;
			modelInfo = new MethodBasedModelInfoWrapper(originalModelInfo);
			((MethodBasedModelInfoWrapper)modelInfo).setSelected(featureName);

			getNewClauses(terminalA,terminalB,terminalComp,explicitCompInfo,explicitClauses,newRequires,newEnsures,true);
			getNewClauses(terminalA,terminalB,terminalComp,conjunctiveCompInfo,conjunctiveClauses,newRequires,newEnsures,true);
			getNewClauses(terminalA,terminalB,terminalComp,consecutiveCompInfo,consecutiveClauses,newRequires,newEnsures,true);
			getNewClauses(terminalA,terminalB,terminalComp,cumulativeCompInfo,cumulativeClauses,newRequires,newEnsures,true);
			getNewClauses(terminalA,terminalB,terminalComp,plainCompInfo,plainClauses,newRequires,newEnsures,true);
			
			modelInfo = originalModelInfo;
			
			for (FSTTerminal requiresClause : newRequires){
				compBodyRequires += "\r\n\t" + requiresClause.getBody() + ";";
			}
			for (FSTTerminal ensuresClause : newEnsures){
				compBodyEnsures += "\r\n\t" + ensuresClause.getBody() + ";";
			}
		} else { // if (compMethodChanged) { ...
			List<FSTTerminal> newRequires = new LinkedList<FSTTerminal>();
			List<FSTTerminal> newEnsures = new LinkedList<FSTTerminal>();
			MethodBasedModelInfoWrapper specialModelInfo = new MethodBasedModelInfoWrapper(modelInfo);
			FeatureModelInfo originalModelInfo = modelInfo;
			modelInfo = specialModelInfo;
			setSelectedRejectedFromCompInfo(specialModelInfo,explicitCompInfo);
			getNewClauses(terminalA,terminalB,terminalComp,explicitCompInfo,explicitClauses,newRequires,newEnsures,false);
			if (newRequires.size() > 0)
				compBodyRequires += "requires " + explicitCompInfo[0] + "(" + explicitCompInfo[1] + ");";
			for (FSTTerminal requiresClause : newRequires){
				compBodyRequires += "\r\n\t" + requiresClause.getBody() + ";";
			}
			newRequires.clear();
			if (newEnsures.size() > 0)
				compBodyEnsures += "ensures " + explicitCompInfo[0] + "(" + explicitCompInfo[1] + ");";
			for (FSTTerminal ensuresClause : newEnsures){
				compBodyEnsures += "\r\n\t" + ensuresClause.getBody() + ";";
			}
			newEnsures.clear();
			

			setSelectedRejectedFromCompInfo(specialModelInfo,conjunctiveCompInfo);
			getNewClauses(terminalA,terminalB,terminalComp,conjunctiveCompInfo,conjunctiveClauses,newRequires,newEnsures,false);
			if (newRequires.size() > 0)
				compBodyRequires += "requires " + conjunctiveCompInfo[0] + "(" + conjunctiveCompInfo[1] + ");";
			for (FSTTerminal requiresClause : newRequires){
				compBodyRequires += "\r\n\t" + requiresClause.getBody() + ";";
			}
			newRequires.clear();
			if (newEnsures.size() > 0)
				compBodyEnsures += "ensures " + conjunctiveCompInfo[0] + "(" + conjunctiveCompInfo[1] + ");";
			for (FSTTerminal ensuresClause : newEnsures){
				compBodyEnsures += "\r\n\t" + ensuresClause.getBody() + ";";
			}
			newEnsures.clear();
			

			setSelectedRejectedFromCompInfo(specialModelInfo,consecutiveCompInfo);
			getNewClauses(terminalA,terminalB,terminalComp,consecutiveCompInfo,consecutiveClauses,newRequires,newEnsures,false);
			if (newRequires.size() > 0)
				compBodyRequires += "requires " + consecutiveCompInfo[0] + "(" + consecutiveCompInfo[1] + ");";
			for (FSTTerminal requiresClause : newRequires){
				compBodyRequires += "\r\n\t" + requiresClause.getBody() + ";";
			}
			newRequires.clear();
			if (newEnsures.size() > 0)
				compBodyEnsures += "ensures " + consecutiveCompInfo[0] + "(" + consecutiveCompInfo[1] + ");";
			for (FSTTerminal ensuresClause : newEnsures){
				compBodyEnsures += "\r\n\t" + ensuresClause.getBody() + ";";
			}
			newEnsures.clear();
			

			setSelectedRejectedFromCompInfo(specialModelInfo,cumulativeCompInfo);
			getNewClauses(terminalA,terminalB,terminalComp,cumulativeCompInfo,cumulativeClauses,newRequires,newEnsures,false);
			if (newRequires.size() > 0)
				compBodyRequires += "requires " + cumulativeCompInfo[0] + "(" + cumulativeCompInfo[1] + ");";
			for (FSTTerminal requiresClause : newRequires){
				compBodyRequires += "\r\n\t" + requiresClause.getBody() + ";";
			}
			newRequires.clear();
			if (newEnsures.size() > 0)
				compBodyEnsures += "ensures " + cumulativeCompInfo[0] + "(" + cumulativeCompInfo[1] + ");";
			for (FSTTerminal ensuresClause : newEnsures){
				compBodyEnsures += "\r\n\t" + ensuresClause.getBody() + ";";
			}
			newEnsures.clear();
			

			setSelectedRejectedFromCompInfo(specialModelInfo,plainCompInfo);
			getNewClauses(terminalA,terminalB,terminalComp,plainCompInfo,plainClauses,newRequires,newEnsures,false);
			if (newRequires.size() > 0)
				compBodyRequires += "requires " + plainCompInfo[0] + "(" + plainCompInfo[1] + ");";
			for (FSTTerminal requiresClause : newRequires){
				compBodyRequires += "\r\n\t" + requiresClause.getBody() + ";";
			}
			newRequires.clear();
			if (newEnsures.size() > 0)
				compBodyEnsures += "ensures " + plainCompInfo[0] + "(" + plainCompInfo[1] + ");";
			for (FSTTerminal ensuresClause : newEnsures){
				compBodyEnsures += "\r\n\t" + ensuresClause.getBody() + ";";
			}
			newEnsures.clear();
			modelInfo = originalModelInfo;
		} // if (compMethodChanged) {} else { ...
		
		terminalComp.setBody(compBodyRequires + compBodyEnsures);
	}

	@Override
	protected void plainContracting(FSTTerminal terminalA, FSTTerminal terminalB,
			FSTTerminal terminalComp) {
		List<FSTTerminal> reqClausesB = getRequiresClauses(terminalB);
		String newOrOriginal = "";
		String oldOrOriginal = "";
		for (FSTTerminal clause: reqClausesB){
			if (clause.getBody().contains(REQUIRE_OR_ORIGINAL)){
				oldOrOriginal = clause.getBody();
				newOrOriginal = getNewReqOrOriginal(clause.getBody(), getFeatureState(terminalA));
				break;
			}
		}
		
		String bodyB = terminalB.getBody();
		if (bodyB.contains(FINAL_CONTRACT)){
			terminalComp.setBody(bodyB.replace(oldOrOriginal, newOrOriginal));
			return;
		}

		String featureName = getFeatureName(terminalA);
		boolean isObligatory = modelInfo.isObligatoryForMethod(getClassName(terminalA), getMethodName(terminalA), featureName);
		
		if (isObligatory && terminalB.getBody().trim().isEmpty()){
			terminalComp.setBody(
						"require FM.FeatureModel." 
					+ 	getFeatureState(terminalA)
					+ 	" || " + REQUIRE_OR_ORIGINAL
					+	";"
					+	terminalA.getBody() 
					+	FINAL_CONTRACT);
			return;
		}
		
		
		List<FSTTerminal> reqClausesA = getRequiresClauses(terminalA);
		List<FSTTerminal> ensClausesA = getEnsuresClauses(terminalA);
		List<FSTTerminal> ensClausesB = getEnsuresClauses(terminalB);
		
		String bodyComp = FINAL_CONTRACT;
		String pre = "";
		String post = "";
		if (!isObligatory){
			bodyComp = "";
			pre = "FM.FeatureModel." + getFeatureState(terminalA) + " ==> (";
			post = ")";
		}
		
		Set<String> features = getFeatures(reqClausesB, "requires");
		features.addAll(getFeatures(ensClausesB, "ensures"));
		
		for (String feature : features){
			pre += "!FM.FeatureModel." + feature + " ==> (";
			post += ")";
		}
		
		for (FSTTerminal clause : reqClausesB){
			if (clause.getBody().contains(REQUIRE_OR_ORIGINAL)){
				bodyComp += newOrOriginal;
				continue;
			}
			bodyComp += "\r\n\trequires " + clause.getBody().replaceAll("requires ", "") + ";";
		}
		
		for (FSTTerminal clause : reqClausesA) {
			bodyComp += "\r\n\trequires "
					 + pre
					 + clause.getBody().replaceAll("requires ", "")
					 + post
					 + ";";
		}
		
		for (FSTTerminal clause : ensClausesB){
			bodyComp += "\r\n\tensures " + clause.getBody().replaceAll("ensures ", "") + ";";
		}
		
		for (FSTTerminal clause : ensClausesA) {
			bodyComp += "\r\n\tensures "
					 + pre
					 + clause.getBody().replaceAll("ensures ", "")
					 + post
					 + ";";
		}
		
		terminalComp.setBody(bodyComp);
		
	}

	protected void contractOverriding(FSTTerminal terminalA,
			FSTTerminal terminalB, FSTTerminal terminalComp) {
		
		String featureNameA = getFeatureName(terminalA);
		String featureStateA = getFeatureState(terminalA);

		List<FSTTerminal> reqClaB = getRequiresClauses(terminalB);
		
		if (modelInfo.isObligatoryForMethod(getClassName(terminalA),getMethodName(terminalA),featureNameA)){
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
				
				
				//List<String> selected = getSelectedFeatures(requiresBody);
				//List<String> rejected = getRejectedFeatures(requiresBody);
				modelInfo.reset();				
				selectFeaturesFromClause(requiresBody);
				rejectFeaturesFromClause(requiresBody);
				
				//rejected.add(featureNameA);
				//if (modelInfo.hasValidProduct(selected, rejected)){
				if (modelInfo.isRejectable(featureNameA)){
					// only add if combination could be possible (allredy selected and rejected Features plus reject Feature of TerminalB can lead to a valid Product)
					// TODO: prüfen, ib man das hier evtl. noch vereinfachen kann!
					//		 möglicherweise lassen sich die Implikationen noch reduzieren 
					//		 evtl ist auch die Klausel immer wahr, wenn Feature gewählt wurde, 
					//		 aber !Feature nicht in die Klausel eingefügt wird (!Feature impliziert
					//		 dass einer der Ausdrücke in den folgenden Implikationen immer flase ist...
					terminalCompBody += "\r\n\t requires !FM.FeatureModel." + featureStateA + " ==> (" + requiresBody + ");";
				}
			}
			
			// new requires-clauses
			for (FSTTerminal requiresA : reqClaA){
				terminalCompBody += "\r\n\t requires FM.FeatureModel." + featureStateA + " ==> (" + requiresA.getBody() + ");";
			}
			
			// old ensures-clauses
			for (FSTTerminal ensuresB : ensClaB){
				//List<String> selected = getSelectedFeatures(ensuresB.getBody());
				//List<String> rejected = getRejectedFeatures(ensuresB.getBody());
				modelInfo.reset();
				selectFeaturesFromClause(ensuresB.getBody());
				rejectFeaturesFromClause(ensuresB.getBody());
				
				//rejected.add(featureNameA);				
				//if (modelInfo.hasValidProduct(selected, rejected)){
				if (modelInfo.isRejectable(featureNameA)){
					// only add if combination could be possible (allredy selected and rejected Features plus reject Feature of TerminalB can lead to a valid Product)
					// TODO: siehe requires...
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
		boolean isObligatory = modelInfo.isObligatoryForMethod(getClassName(terminalA), methodName,getFeatureName(terminalA));
		
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
								+ clause.getBody().replace(type + " ", "")
								+ post
								+ ";";
				// Klausel nicht hinzufügen, falls Featurekombination nicht möglich ist
				//List<String> selected = getSelectedFeatures(newClause);
				//List<String> rejected = getRejectedFeatures(newClause);
				//if (modelInfo.hasValidProduct(selected, rejected))
				modelInfo.reset();
				selectFeaturesFromClause(newClause);
				rejectFeaturesFromClause(newClause);
				if (modelInfo.isValidSelection())
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
									.replaceAll("requires (FM.FeatureModel.[\\w]+\\(?\\)?)?( \\|\\| FM.FeatureModel.[\\w]+\\(?\\)?)*" 
												+ " \\|\\| " + REQUIRE_OR_ORIGINAL + ";", "")}
							, clauseBody
							, 0
							, type
							).replace(type + " ", "");
			} else {
				result += clauseBody.replace(type + " ", "");
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
		
		if (!modelInfo.isObligatoryForMethod(getClassName(terminalA), getMethodName(terminalA),getFeatureName(terminalA))){
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
			terminalCompBody += "\r\n\tensures " + ensuresB.getBody().replaceAll("ensures ","") + ";";
		}
		
		for (FSTTerminal ensuresA : ensClausesA){
			terminalCompBody += "\r\n\tensures "+ preEnsures + ensuresA.getBody().replaceAll("ensures ","") + postEnsures + ";";
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
		
		if (!modelInfo.isObligatoryForMethod(getClassName(terminalA),getMethodName(terminalA),getFeatureName(terminalA))) {
			preClause = "FM.FeatureModel." + featureState + " ==> (";
			postClause = ")";
		}
				
		for (FSTTerminal requiresB : reqClaB){
			String clauseBody = requiresB.getBody();
			if (clauseBody.contains(REQUIRE_OR_ORIGINAL)){
				terminalCompBody += getNewReqOrOriginal(clauseBody, featureState);
				continue;
			}
			terminalCompBody += "\r\n\trequires " + requiresB.getBody().replaceAll("requires ","") + ";"; 
		}
		
		for (FSTTerminal requiresA : reqClaA){
			terminalCompBody += "\r\n\trequires " + preClause + requiresA.getBody().replaceAll("requires ","") + postClause + ";";
		}
		
		for (FSTTerminal ensuresB : ensClaB){
			terminalCompBody += "\r\n\tensures " + ensuresB.getBody().replaceAll("ensures ","") + ";"; 
		}
		
		for (FSTTerminal ensuresA : ensClaA){
			terminalCompBody += "\r\n\tensures " + preClause + ensuresA.getBody().replaceAll("ensures ","") + postClause + ";";
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
		
		if (!modelInfo.isObligatoryForMethod(getClassName(terminalA),getMethodName(terminalA),getFeatureName(terminalA))){
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
			terminalCompBody += "\r\n\tensures " + ensuresB.getBody().replaceAll("ensures ","") + ";";
		}
		
		for (FSTTerminal ensuresA : ensClaA){
			terminalCompBody += "\r\n\tensures " + preEnsures + ensuresA.getBody().replaceAll("ensures ","") + postEnsures + ";";
		}
		
		terminalComp.setBody(terminalCompBody);
	}

}