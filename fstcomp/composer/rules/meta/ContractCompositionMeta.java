package composer.rules.meta;



import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
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
 * @author Matthias Praast
 *
 */
public class ContractCompositionMeta extends ContractComposition {
	
	// xyz
	// Platzhalter in den Klauseln
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
	
	/**
	 * Translates selectionstate of feature into feature-name.
	 * 
	 * @param state selectionsstate of Features 
	 * @return name of Features
	 */
	private String stateToFeatureName(String state){
		List<String> features = metadata.getFeatures();
		state = state.replaceAll("()","");
		for (String feature : features){
			if (feature.toLowerCase().equals(state))
				return feature;
		}
		return state;
	}

	/**
	 * Selects all features in modelInfo, that are marked as selected ( FeatureModel.&lt;FeatureState&lt; without leading !) 
	 * @param clause clause with implications
	 */
	private void selectFeaturesFromClause(String clause){
		Pattern featurePattern = Pattern.compile("[^!]FM\\.FeatureModel\\.([\\w]+)" + (FSTGenComposerExtension.key ? "" : "\\(\\)") +" ==>");
		Matcher featureMatcher = featurePattern.matcher(" " + clause);
		while (featureMatcher.find()){
			modelInfo.selectFeature(stateToFeatureName(featureMatcher.group(1)));
		}
	}
	
	/**
	 * Unselects all features in modelInfo, that ar marked as unselected ( !FeatureModel.&lt;FeatureState&lt; ) 
	 * @param clause clause with implications
	 */	 
	private void rejectFeaturesFromClause(String clause){
		Pattern featurePattern = Pattern.compile("!FM\\.FeatureModel\\.([\\w]+)" + (FSTGenComposerExtension.key ? "" : "\\(\\)") +" ==>");
		Matcher featureMatcher = featurePattern.matcher(clause);
		while (featureMatcher.find()){
			modelInfo.eliminateFeature(stateToFeatureName(featureMatcher.group(1)));
		}
	}

	/**
	 * Calculates the name of the feature-state for a given {@link FSTNode} below the feature-node (without "FeatureModel.")
	 * @param node the {@link FSTNOde}
	 * @return name of the selection-state of the feature 
	 */
	private static String getFeatureState(FSTNode node) {
		return getFeatureName(node).toLowerCase() + (FSTGenComposerExtension.key ? "" : "()");
	}
	
	/**
	 * Returns the name of the Feature for a {@link FSTNode} below the feature node
	 * @param node the {@link FSTNode}
	 * @return name of the Feature
	 */
	private static String getFeatureName(FSTNode node) {
		if ("Feature".equals(node.getType()))
			return node.getName();
		else
			return getFeatureName(node.getParent());
	}
	
	/**
	 * adds additional selection state to disjunction of features
	 * @param oldRequiresClause old clause with disjunction of features
	 * @param featureState selectionstate to add
	 * @return new clause with added selectionstate
	 */
	private String getNewReqOrOriginal(String oldRequiresClause, String featureState){
		String clauseBody = oldRequiresClause.replaceAll(REQUIRE_OR_ORIGINAL, "FM.FeatureModel." + featureState + " || " + REQUIRE_OR_ORIGINAL);		
		return clauseBody + ";";
	}
	
	/**
	 * Removes clause with disjunction of selection states from specification body
	 * @param body specification body
	 * @return specification body without disjunction of selection states
	 */
	private String removeRequireOrOriginal(String body){
		return body.replaceAll("requires FM.FeatureModel.[\\w]+" + (FSTGenComposerExtension.key ? "" : "\\(\\)") +" \\|\\| " + REQUIRE_OR_ORIGINAL + ";","");
	}
	
	/**
	 * Determines the method name for a {@link FSTNode} of a method specification 
	 * @param node specification node
	 * @return method name
	 */
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
	
	/**
	 * Determines the class name for a {@link FSTNode} of a method specification 
	 * @param node specification node
	 * @return class name
	 */
	private String getClassName(FSTNode node){
		if (node == null)
			return "";
		if (node.getType().contains("ClassDeclaration"))
			return node.getName();
		return getClassName(node.getParent());
	}
	
	/**
	 * Calculates if original clauses must be fulfilled to fulfill new clauses. Removes original in that case
	 * @param clauses clauses that may contain original
	 * @param clauseType type of clauses ("requires" or "ensures")
	 * @return true, if original clauses must be fulfilled
	 */
	private boolean removeAndOriginal(List<FSTTerminal> clauses, String clauseType){
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
	
	/**
	 * Simplifies Clauses. Removes implications if the selectionstate is already implied by an other selectionstate (or combination of selectionstates)  
	 * @param clause clause to simplify
	 * @return simplified clause
	 */
	private String simplifyImplications(String clause){
		String simplifiedClause = clause;
		Pattern pat = Pattern.compile("FM\\.FeatureModel\\.([\\w]+)" + (FSTGenComposerExtension.key ? "" : "\\(\\)") +" ==>");
		Matcher match = pat.matcher(clause);
		modelInfo.reset();
		while (match.find()){
			String featureName = stateToFeatureName(match.group(1));
			char prefChar = match.start() > 0 ? clause.charAt(match.start()-1) : ' ';
			if (prefChar == '!'){
				if (modelInfo.isAlwaysEliminated(featureName))
					simplifiedClause = simplifiedClause.replaceAll("!FM.FeatureModel." + match.group(1).replaceAll("\\(\\)","") + "\\(?\\)? ==> ", "");
				else
					modelInfo.eliminateFeature(featureName);
			} else {
				if (modelInfo.isAlwaysSelected(featureName))
					simplifiedClause = simplifiedClause.replaceAll("([^!])FM.FeatureModel." + match.group(1).replaceAll("\\(\\)","") + "\\(?\\)? ==> ", "$1");
				else
					modelInfo.selectFeature(featureName);
			}			
		}
		
		return simplifiedClause;
	}
	
	/**
	 * Calculates all selectionsstates of features that occur in the given clauses
	 * @param clauses clauses to check for selectionsstates
	 * @param clauseType type of clauses ("requires" or "ensures")
	 * @return Set of selectionsstates of features
	 */
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
	
	/**
	 * Calculates the compositions-keyword for a {@link FSTNode} of a specification
	 * @param terminal specification node
	 * @return composition-keyword
	 */
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
	
	/**
	 * Calculates informations for composition for method-based Contract Refinement
	 * @param clause clause with compostions informatiuons (syntax: "&lt;clausetype&gt; &lt;composition-type&gt;(&lt;list of featurestates&gt;)" )
	 * @param type type of clauses ("requires" or "ensures")
	 * @return String-Array with 4 elements:
	 * <br>0 composition-type
	 * <br>1 list of featurestates (&lt;featurestate1&gt;,...,&lt;featurestaten&gt;)
	 * <br>2 resulting prefix in specification
	 * <br>3 resulting suffix in specification
	 */
	private String[] extractCompositionInformation(String clause,String type){
		String[] result = {"","","",""}; 
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
			for (String status : result[1].split(",")){
				if (status.isEmpty())
					continue;
				if (status.charAt(0) == '!')
					result[2] += "!FM.FeatureModel." + status.substring(1) + " ==> (";
				else
					result[2] += "FM.FeatureModel." + status + " ==> (";
				result[3] += ")";
			}			
		}
			
		return result;
	}
	
	/**
	 * Calculates informations for composition for method-based Contract Refinement
	 * @param clause clause with compostions informatiuons (syntax: "&lt;composition-type&gt;(&lt;list of featurestates&gt;)" )
	 * @return String-Array with 4 elements:
	 * <br>0 composition-type
	 * <br>1 list of featurestates (&lt;featurestate1&gt;,...,&lt;featurestaten&gt;)
	 * <br>2 resulting prefix in specification
	 * <br>3 resulting suffix in specification
	 */
	private String[] extractCompositionInformation(String clause){
		String[] result = {"","","",""}; 
		Pattern p = Pattern.compile(
					"(" + COMPOSITION_CONJUNCTIVE + "|" + COMPOSITION_CONSECUTIVE + "|" + COMPOSITION_CUMULATIVE + "|"
						+ COMPOSITION_EXPLICIT + "|" + COMPOSITION_PLAIN + ")\\("
				+	"((!?[\\w]+\\(?\\)?)?(,(!?[\\w]+\\(?\\)?))*)"
				+ 	"\\)"
				);
		Matcher m = p.matcher(clause);
		if (m.find()){
			result[0] = m.group(1);
			result[1] = m.group(2);
			for (String status : result[1].split(",")){
				if (status.isEmpty())
					continue;
				if (status.charAt(0) == '!')
					result[2] += "!FM.FeatureModel." + status.substring(1) + " ==> (";
				else
					result[2] += "FM.FeatureModel." + status + " ==> (";
				result[3] += ")";
			}
			
		}
			
		return result;
	}
	
	/**
	 * selects/unselects Features in a {@link FeatureModelInfo} from a composition information array
	 * @param specialModelInfo object of {@link FeatureModelInfo} to select/unselect features
	 * @param compInfo composition information array
	 */
	private void setSelectedRejectedFromCompInfo(MethodBasedModelInfoWrapper specialModelInfo,String[] compInfo){
		specialModelInfo.clear();
		if (compInfo == null)
			return;
		
		for (String status : compInfo[1].split(",")){
			if (status.isEmpty())
				continue;
			if (status.charAt(0) == '!')
				specialModelInfo.setRejected(stateToFeatureName(status.substring(1)));
			else
				specialModelInfo.setSelected(stateToFeatureName(status));
		}
	}

	@Override
	public void compose(FSTTerminal terminalA, FSTTerminal terminalB,
			FSTTerminal terminalComp, FSTNonTerminal nonterminalParent)
			throws CompositionException {
		
		// compose the first specification with an empty one 
		if (terminalB.getBody().contains("\\not_composed\r\n")){
			FSTTerminal newComp = (FSTTerminal)terminalB.getDeepClone();
			FSTTerminal newB = (FSTTerminal)terminalB.getDeepClone();
			newComp.setParent(terminalB.getParent());
			newB.setParent(terminalB.getParent());
			if (modelInfo.isCoreFeature(getFeatureName(terminalB)))
				newB.setBody( contractStyle.equals(METHOD_BASED_COMPOSITION) ? "\r\n\trequires " + COMPOSITION_EXPLICIT + "();" : "");
			else
				newB.setBody("\r\n\trequires " + REQUIRE_OR_ORIGINAL + ";"
						+ (contractStyle.equals(METHOD_BASED_COMPOSITION) ? "\r\n\trequires " + COMPOSITION_EXPLICIT + "();" : "")
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
		
		String body = terminal.getBody();
		if (modelInfo.isCoreFeature(getFeatureName(terminal)))
			terminal.setBody(
					  "\\not_composed\r\n\t" 
					+ body);
		else
			terminal.setBody(
					  "\\not_composed\r\n\trequires FM.FeatureModel." 
					+ getFeatureState(terminal) 
					+ " || " + REQUIRE_OR_ORIGINAL + ";\r\n\t" 
					+ body);
		return;
	}
	 
	@Override
	public void postCompose(FSTTerminal terminal) {
		String body = terminal.getBody();
		if (removeRequireOrOriginal(body).trim().isEmpty()) {
			terminal.setBody("");
			return;
		}
		
		if (FSTGenComposerExtension.key && body.replaceAll("@", "").trim().isEmpty()) { 
			return;
		}
		
		body = body.replaceAll(" \\|\\| " + REQUIRE_OR_ORIGINAL, "");
		body = body.replaceAll(FINAL_CONTRACT, "");
		body = body.replaceAll("requires  \\|\\| ", "");
		body = body.replaceAll("\\" + ORIGINAL_KEYWORD, "true");
		body = body.replaceAll("\\\\not_composed", "");
		if (FSTGenComposerExtension.key) {
			body = "  @ requires " + modelInfo.getValidClause() + ";\r\n\t" + body;
		} else {
			body = "  @ " + body;
		}

		terminal.setBody(body);
		
		if (contractStyle.equals(METHOD_BASED_COMPOSITION)){
			postComposeMethodBased(terminal);
		}
		
		body = terminal.getBody();
		
		while (body.contains("  "))
			body = body.replaceAll("  ", " ");
		
		while (body.contains("\r\n\t\r\n\t") || body.contains("\r\n\t \r\n\t"))
			body = body.replaceAll("\r\n[\\s]*\r\n\t", "\r\n\t");

		body = body.replaceAll("\r\n\t([\\w])", "\r\n\t $1");
		body = body.replaceAll("\r\n\t([\\s]*)", "\r\n\t  @$1");
		
		if (!body.endsWith("\r\n\t ")) {
			body = body + "\r\n\t ";
		}
		terminal.setBody(body);
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
	
	// Changed to get Clauses without Semicolon, skip super-call if empty body (otherwise would get an error)
	@Override
	protected List<FSTTerminal> getRequiresClauses(FSTTerminal terminal){
		return getRequiresClauses(terminal, false);
	}
	

	protected List<FSTTerminal> getRequiresClauses(FSTTerminal terminal,boolean keepSemi){
		if (terminal.getBody().replaceAll("\r","").replaceAll("\n","").replaceAll("\t","").trim().isEmpty())
			return new ArrayList<FSTTerminal>();
		List<FSTTerminal> clauses = super.getRequiresClauses(terminal);
		if (keepSemi)
			return clauses;
		for (FSTTerminal clause : clauses) {
			clause.setBody(clause.getBody().substring(0, clause.getBody().length() - 1));
		}
		return clauses;
		
		
	}
	
	// Changed to get Clauses without Semicolon, skip super-call if empty body (otherwise would get an error)
	@Override
	protected List<FSTTerminal> getEnsuresClauses(FSTTerminal terminal){
		return getEnsuresClauses(terminal,false);
	}

	protected List<FSTTerminal> getEnsuresClauses(FSTTerminal terminal, boolean keepSemi){
		if (terminal.getBody().replaceAll("\r","").replaceAll("\n","").replaceAll("\t","").trim().isEmpty())
			return new ArrayList<FSTTerminal>();
		List<FSTTerminal> clauses = super.getEnsuresClauses(terminal);
		if (keepSemi)
			return clauses;
		for (FSTTerminal clause : clauses) {
			clause.setBody(clause.getBody().substring(0, clause.getBody().length() - 1));
		}
		return clauses;
	}

	/**
	 * Adds Clauses to a Hash-Map. Hash-Map maps from composition information string to according clauses
	 * @param clauses clauses to add
	 * @param hashMap HashMap to add clauses to
	 */
	private void addClausesToHashMap(List<FSTTerminal> clauses,HashMap<String,List<FSTTerminal>> hashMap){
		List<FSTTerminal> currentEntry = null;
		for (FSTTerminal clause : clauses){
			String clauseBody = clause.getBody();
			if (clauseBody.contains(REQUIRE_OR_ORIGINAL)){
				LinkedList<FSTTerminal> reqOrOriginal = new LinkedList<FSTTerminal>();
				reqOrOriginal.add(clause);
				hashMap.put(REQUIRE_OR_ORIGINAL, reqOrOriginal);
				continue;
			}
			if (clauseBody.contains(COMPOSITION_CONJUNCTIVE) || clauseBody.contains(COMPOSITION_CONSECUTIVE) || clauseBody.contains(COMPOSITION_CUMULATIVE)
					|| clauseBody.contains(COMPOSITION_EXPLICIT) || clauseBody.contains(COMPOSITION_PLAIN)){
				//currentEntry = null;
				currentEntry = hashMap.get(clauseBody.replaceAll("requires", "").replaceAll("ensures", "").trim());
				if (currentEntry == null){
					currentEntry = new LinkedList<FSTTerminal>();
					hashMap.put(clauseBody.replaceAll("requires", "").replaceAll("ensures", "").trim(), currentEntry);
				}
				continue;
			}
			
			if (currentEntry == null){
				// should not happen
				continue;
			}
			
			currentEntry.add(clause);
		}
	}
	
	/**
	 * writes clauses into a String-Array with length 2 (requires-clauses and ensures-clauses)
	 * @param clauses to put into the Array
	 * @return String-Array (requires-clauses and ensures-clauses)
	 */
	private String[] clauseListToStrings(List<FSTTerminal> clauses){
		String[] result = {"",""};
		
		for (FSTTerminal clause : clauses){
			if (clause.getBody().contains(COMPOSITION_CONJUNCTIVE) || clause.getBody().contains(COMPOSITION_CONSECUTIVE) 
				|| clause.getBody().contains(COMPOSITION_CUMULATIVE) || clause.getBody().contains(COMPOSITION_EXPLICIT) 
				|| clause.getBody().contains(COMPOSITION_PLAIN))
				continue;
			if (clause.getBody().contains(REQUIRE_OR_ORIGINAL))
				continue;
			if (clause.getBody().contains("requires "))
				result[0]+= clause.getBody() + ";\r\n\t";
			else if (clause.getBody().contains("ensures "))
				result[1] +=  clause.getBody() + ";\r\n\t";
		}
		
		return result;
	}
	
	private String getCompMethodByCompositionKey(String compositionKey){
		if (compositionKey.equals(CompositionKeyword.CONJUNCTIVE_CONTRACT.getLabel()))
			return COMPOSITION_CONJUNCTIVE;
		if (compositionKey.equals(CompositionKeyword.CONSECUTIVE_CONTRACT.getLabel()))
			return COMPOSITION_CONSECUTIVE;
		if (compositionKey.equals(CompositionKeyword.CUMULATIVE_CONTRACT.getLabel()))
			return COMPOSITION_CUMULATIVE;
		if (compositionKey.equals(CompositionKeyword.FINAL_CONTRACT.getLabel())|| compositionKey.equals(CompositionKeyword.FINAL_METHOD.getLabel()))
			return COMPOSITION_PLAIN;
		return COMPOSITION_EXPLICIT;
	}
	
	private String getCompMethodForTerminal(FSTTerminal terminal){
		String compositionKey = getCompositionKey(terminal);
		if (compositionKey.equals(CompositionKeyword.CONJUNCTIVE_CONTRACT.getLabel()))
			return COMPOSITION_CONJUNCTIVE;
		if (compositionKey.equals(CompositionKeyword.CONSECUTIVE_CONTRACT.getLabel()))
			return COMPOSITION_CONSECUTIVE;
		if (compositionKey.equals(CompositionKeyword.CUMULATIVE_CONTRACT.getLabel()))
			return COMPOSITION_CUMULATIVE;
		if (compositionKey.equals(CompositionKeyword.FINAL_CONTRACT.getLabel())|| compositionKey.equals(CompositionKeyword.FINAL_METHOD.getLabel()))
			return COMPOSITION_PLAIN;
		if (compositionKey.equals(CompositionKeyword.EXPLICIT_CONTRACT.getLabel()))
			return COMPOSITION_EXPLICIT;
		return "";
	}
	
	private boolean newCompMethod(String[] compInfo, FSTTerminal terminal){
		String compKey = getCompositionKey(terminal);
		if (compKey.equals(""))
			return false;
		return !compInfo[0].equals(getCompMethodByCompositionKey(compKey));
	}
	
	protected void composeByKey(FSTTerminal terminalA,
			FSTTerminal terminalB, FSTTerminal terminalComp,
			String compMethod) {
		if (compMethod.equals(COMPOSITION_CONJUNCTIVE))
			conjunctiveContracting(terminalA, terminalB, terminalComp);
		else if (compMethod.equals(COMPOSITION_CONSECUTIVE))
			consecutiveContracting(terminalA, terminalB, terminalComp);
		else if (compMethod.equals(COMPOSITION_CUMULATIVE))
			cumulativeContracting(terminalA, terminalB, terminalComp);
		else if (compMethod.equals(COMPOSITION_PLAIN))
			plainContracting(terminalA, terminalB, terminalComp);
		else if (compMethod.equals(COMPOSITION_EXPLICIT))
			explicitContracting(terminalA, terminalB, terminalComp);
			
	}
	
	protected void compositionByKeywords(FSTTerminal terminalA,
			FSTTerminal terminalB, FSTTerminal terminalComp,
			FSTNonTerminal nonterminalParent) {
		
		// first get all old Variants
		
		HashMap<String,List<FSTTerminal>> variants = new HashMap<String,List<FSTTerminal>>();
		List<FSTTerminal> reqClausesB = getRequiresClauses(terminalB);
		List<FSTTerminal> ensClausesB = getEnsuresClauses(terminalB);
		String bodyA = terminalA.getBody();

		addClausesToHashMap(reqClausesB,variants);
		addClausesToHashMap(ensClausesB,variants);
		
		FeatureModelInfo originalModelInfo = modelInfo;
		modelInfo = new MethodBasedModelInfoWrapper(originalModelInfo);
		
		FSTTerminal variantB = (FSTTerminal) terminalB.getDeepClone();
		variantB.setParent(terminalB.getParent());
		FSTTerminal variantComp = (FSTTerminal) terminalComp.getDeepClone();
		variantComp.setParent(terminalComp.getParent());
		
		String newReqOrOriginal = "";
		String compRequires = "";
		String compEnsures = "";
		
		String featureName = getFeatureName(terminalA);
		String featureState = getFeatureState(terminalA);
		
		boolean obligatory = originalModelInfo.isMethodCoreFeature(getClassName(terminalA),getMethodName(terminalA),featureName);
		
		for (Map.Entry<String,List<FSTTerminal>> variantEntry : variants.entrySet()){
			if (variantEntry.getKey().contains(REQUIRE_OR_ORIGINAL)){
				if (!originalModelInfo.isCoreFeature(featureName))
					newReqOrOriginal = getNewReqOrOriginal(variantEntry.getValue().get(0).getBody(), getFeatureState(terminalA));
				continue;
			}
			String[] compInfo =  extractCompositionInformation(variantEntry.getKey());
			String[] oldBodyStrings = clauseListToStrings(variantEntry.getValue());
			setSelectedRejectedFromCompInfo((MethodBasedModelInfoWrapper)modelInfo, compInfo);
			if (newCompMethod(compInfo,terminalA)){
				// new Compositions-Method -> FeatureState must be inserted into compInfo
				// 1. Feature not selected:
				if (!obligatory && modelInfo.canBeEliminated(featureName)){
					compRequires += "\r\n\trequires " + compInfo[0] + "(!" + featureState + (compInfo[1].isEmpty()?"":"," + compInfo[1]) + ");\r\n\t"
							     +  oldBodyStrings[0];
					compEnsures  += "\r\n\tensures "  + compInfo[0] + "(!" + featureState + (compInfo[1].isEmpty()?"":"," + compInfo[1]) + ");\r\n\t"
							     +  oldBodyStrings[1];
				}
				// 2. Feature selected
				if (modelInfo.canBeSelected(featureName)) {
					// Compose with old method
					variantB.setBody(oldBodyStrings[0] + oldBodyStrings[1].trim());
					variantComp.setBody("");
					((MethodBasedModelInfoWrapper)modelInfo).setSelected(featureName);
					composeByKey(terminalA,variantB,variantComp,compInfo[0]);
					terminalA.setBody(bodyA);
					
					if (variantComp.getBody().trim().isEmpty())
						continue;
					
					// write new Composition-Method
					String compMethod = getCompMethodForTerminal(terminalA);
					compRequires += "\r\n\trequires " + compMethod + "(" + featureState + (compInfo[1].isEmpty()?"":"," + compInfo[1]) + ");";
					compEnsures  += "\r\n\tensures "  + compMethod + "(" + featureState + (compInfo[1].isEmpty()?"":"," + compInfo[1]) + ");";
					
					// write new Clauses
					List<FSTTerminal> reqClausesComp = getRequiresClauses(variantComp);
					List<FSTTerminal> ensClausesComp = getEnsuresClauses(variantComp);
					for (FSTTerminal clause : reqClausesComp)
						compRequires += "\r\n\t" + clause.getBody() + ";";
					for (FSTTerminal clause : ensClausesComp)
						compEnsures  += "\r\n\t" + clause.getBody() + ";";
				}
			} else {
				// no new Coposition-Method
				// only add, when Feature can be selected, otherwise ignore
				if (modelInfo.canBeSelected(featureName)){
					// call composition
					variantB.setBody(oldBodyStrings[0] + oldBodyStrings[1].trim());
					variantComp.setBody("");
					composeByKey(terminalA,variantB,variantComp,compInfo[0]);
					terminalA.setBody(bodyA);
					
					if (variantComp.getBody().trim().isEmpty())
						continue;
					
					// write compostion-Method to Result
					compRequires += "\r\n\trequires " + compInfo[0] + "(" + compInfo[1] + ");";
					compEnsures  += "\r\n\tensures "  + compInfo[0] + "(" + compInfo[1] + ");";
					
					// Add new Clauses to result
					List<FSTTerminal> reqClausesComp = getRequiresClauses(variantComp);
					List<FSTTerminal> ensClausesComp = getEnsuresClauses(variantComp);
					for (FSTTerminal clause : reqClausesComp)
						compRequires += "\r\n\t" + clause.getBody() + ";";
					for (FSTTerminal clause : ensClausesComp)
						compEnsures  += "\r\n\t" + clause.getBody() + ";";
				}
			}
		}
		
		terminalComp.setBody(newReqOrOriginal + "\r\n\t" + compRequires + "\r\n\t" + compEnsures);
		modelInfo = originalModelInfo;
	}
	
	@Override
	protected void plainContracting(FSTTerminal terminalA, FSTTerminal terminalB,
			FSTTerminal terminalComp) {
		List<FSTTerminal> reqClausesB = getRequiresClauses(terminalB);
		String newOrOriginal = "";
		String oldOrOriginal = "";
		String featureName = getFeatureName(terminalA);
		for (FSTTerminal clause: reqClausesB){
			if (clause.getBody().contains(REQUIRE_OR_ORIGINAL)){
				if (!modelInfo.isCoreFeature(featureName)){
					oldOrOriginal = clause.getBody();
					newOrOriginal = getNewReqOrOriginal(clause.getBody(), getFeatureState(terminalA));
				}
				break;
			}
		}
		
		String bodyB = terminalB.getBody();
		if (bodyB.contains(FINAL_CONTRACT)){
			terminalComp.setBody(bodyB.replace(oldOrOriginal, newOrOriginal));
			return;
		}

		boolean isObligatory = modelInfo.isMethodCoreFeature(getClassName(terminalA), getMethodName(terminalA), featureName);
		
		if (isObligatory && terminalB.getBody().trim().isEmpty()){
			if (!modelInfo.isCoreFeature(featureName))
				terminalComp.setBody(
							newOrOriginal
						+	"\r\n\t "
						+	terminalA.getBody() 
						+	FINAL_CONTRACT);
			else
				terminalComp.setBody(
						terminalA.getBody() 
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
				if (!modelInfo.isCoreFeature(featureName))
					bodyComp += newOrOriginal;
				continue;
			}
			bodyComp += "\r\n\t " + clause.getBody() + ";";
		}
		
		for (FSTTerminal clause : reqClausesA) {
			bodyComp += "\r\n\t requires "
					 + pre
					 + clause.getBody().replaceAll("requires ", "")
					 + post
					 + ";";
		}
		
		for (FSTTerminal clause : ensClausesB){
			bodyComp += "\r\n\t " + clause.getBody() + ";";
		}
		
		for (FSTTerminal clause : ensClausesA) {
			bodyComp += "\r\n\t ensures "
					 + pre
					 + clause.getBody().replaceAll("ensures ", "")
					 + post
					 + ";";
		}
		
		terminalComp.setBody(bodyComp);
		
	}

	protected void contractOverriding(FSTTerminal terminalA,
			FSTTerminal terminalB, FSTTerminal terminalComp) {
		
		// special case of explicit Contracting
		// -> only no \original
		// maybe should be checked...
		
		explicitContracting(terminalA, terminalB, terminalComp);
		
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
		boolean isObligatory = modelInfo.isMethodCoreFeature(getClassName(terminalA), methodName,getFeatureName(terminalA));
		
		String bodyComp = "";
		
		for (FSTTerminal clause : reqClausesB){
			String clauseBody = clause.getBody();
			if (clauseBody.contains(REQUIRE_OR_ORIGINAL)){
				clause.setBody("");
				if (!modelInfo.isCoreFeature(getFeatureName(terminalA)))
					bodyComp += getNewReqOrOriginal(clauseBody, featureState);
				break;
			}
		}
		
		bodyComp += explicitComposeClauses(reqClausesB, reqClausesA, 
				terminalB, "requires", featureState, isObligatory);
		
		bodyComp += explicitComposeClauses(ensClausesB, ensClausesA, 
				terminalB, "ensures", featureState, isObligatory);
		
		
			
		terminalComp.setBody(bodyComp);
	}
	
	private String explicitComposeClauses(List<FSTTerminal> originalClauses, List<FSTTerminal> newClauses, 
			FSTTerminal originalTerminal, String type, String featureState, boolean isObligatory){

		String result = "";
		String pre = "";
		String post = "";
		boolean andOriginal = removeAndOriginal(newClauses, type);
		// only implication, when not extended (no "requires \original"; or similar)
		if (!andOriginal){
			pre = "!FM.FeatureModel." + featureState + " ==> (";
			post = ")";
			
		}
		
		// skip original clauses if Feature is obligatory for the method
		// except: FeatureA is obligatory but extends the spezification
		if (!isObligatory || andOriginal) {
			for (FSTTerminal clause : originalClauses){
				if (clause.getBody().trim().isEmpty())
					continue;
				String newClause = "\r\n\t "
								+ type
								+ " "
								+ pre
								+ clause.getBody().replace(type + " ", "")
								+ post
								+ ";";
				// don't add Clause if feature-combination is not possible
				modelInfo.reset();
				selectFeaturesFromClause(newClause);
				rejectFeaturesFromClause(newClause);
				if (modelInfo.isValidSelection())
					result += simplifyImplications(newClause);
			}
		}
		
		// always add new clauses
		// when not obligatory: with implications
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
			result += "\r\n\t "
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
		List<FSTTerminal> reqClausesA = getRequiresClauses(terminalA,true);
		List<FSTTerminal> reqClausesB = getRequiresClauses(terminalB,true);
		List<FSTTerminal> ensClausesA = getEnsuresClauses(terminalA); 
		List<FSTTerminal> ensClausesB = getEnsuresClauses(terminalB);
		
		// Search for the REquires or original-Clause
		for (FSTTerminal clause : reqClausesB){
			String clauseBody = clause.getBody();
			clauseBody = clauseBody.substring(0, clauseBody.length()-1);
			if (clauseBody.contains(REQUIRE_OR_ORIGINAL)){
				if (!modelInfo.isCoreFeature(getFeatureName(terminalA)))
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
		
		if (!modelInfo.isMethodCoreFeature(getClassName(terminalA), getMethodName(terminalA),getFeatureName(terminalA))){
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
			terminalCompBody += "\r\n\t requires " + terminalBRequires + "\r\n\t\t || " + preRequires + terminalARequires + postRequires + ";";
		} else if (!terminalARequires.trim().isEmpty()){
			terminalCompBody += "\r\n\t requires " + preRequires + terminalARequires + postRequires + ";";
		} else if (!terminalBRequires.trim().isEmpty()) {
			terminalCompBody += "\r\n\t requires " + terminalBRequires + ";";
		}
		
		for (FSTTerminal ensuresB : ensClausesB){
			terminalCompBody += "\r\n\t " + ensuresB.getBody() + ";";
		}
		
		for (FSTTerminal ensuresA : ensClausesA){
			terminalCompBody += "\r\n\t ensures "+ preEnsures + ensuresA.getBody().replaceAll("ensures ","") + postEnsures + ";";
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
		
		if (!modelInfo.isMethodCoreFeature(getClassName(terminalA),getMethodName(terminalA),getFeatureName(terminalA))) {
			preClause = "FM.FeatureModel." + featureState + " ==> (";
			postClause = ")";
		}
				
		for (FSTTerminal requiresB : reqClaB){
			String clauseBody = requiresB.getBody();
			if (clauseBody.contains(REQUIRE_OR_ORIGINAL)){
				if (!modelInfo.isCoreFeature(getFeatureName(terminalA)))
					terminalCompBody += getNewReqOrOriginal(clauseBody, featureState);
				continue;
			}
			terminalCompBody += "\r\n\t " + requiresB.getBody() + ";"; 
		}
		
		for (FSTTerminal requiresA : reqClaA){
			terminalCompBody += "\r\n\t requires " + preClause + requiresA.getBody().replaceAll("requires ","") + postClause + ";";
		}
		
		for (FSTTerminal ensuresB : ensClaB){
			terminalCompBody += "\r\n\t " + ensuresB.getBody() + ";"; 
		}
		
		for (FSTTerminal ensuresA : ensClaA){
			terminalCompBody += "\r\n\t ensures " + preClause + ensuresA.getBody().replaceAll("ensures ","") + postClause + ";";
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
				if (!modelInfo.isCoreFeature(getFeatureName(terminalA)))
					terminalCompBody = getNewReqOrOriginal(clauseBody, featureState);
				reqClaB.remove(clause);
				break;
			}
		}
		
		
		String requiresA = reqClaA.size() > 0 ? joinClause(reqClaA,"requires") : "";
		String requiresB = reqClaB.size() > 0 ? joinClause(reqClaB,"requires") : "";
		
		if (!modelInfo.isMethodCoreFeature(getClassName(terminalA),getMethodName(terminalA),getFeatureName(terminalA))){
			preRequires = "FM.FeatureModel." + featureState + " && ";
			postRequires = "";
			preEnsures = "FM.FeatureModel." + featureState + " ==> (";
			postEnsures = ")";
		}
		
		if (!requiresA.trim().isEmpty() && !requiresB.trim().isEmpty()){
			terminalCompBody += "\r\n\t requires " + requiresB + "\r\n\t\t || (" + preRequires + requiresA + postRequires + ");";
		} else if (!requiresA.trim().isEmpty()){
			terminalCompBody += "\r\n\t requires " + preRequires + requiresA + postRequires + ";";
		} else if (!requiresB.trim().isEmpty()) {
			terminalCompBody += "\r\n\t requires " + requiresB + ");";
		}
		
		for (FSTTerminal ensuresB : ensClaB){
			terminalCompBody += "\r\n\t " + ensuresB.getBody() + ";";
		}
		
		for (FSTTerminal ensuresA : ensClaA){
			terminalCompBody += "\r\n\t ensures " + preEnsures + ensuresA.getBody().replaceAll("ensures ","") + postEnsures + ";";
		}
		
		terminalComp.setBody(terminalCompBody);
	}

}