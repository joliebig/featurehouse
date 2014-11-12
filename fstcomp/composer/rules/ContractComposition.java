package composer.rules;

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import composer.CompositionException;
import composer.FSTGenComposer;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class ContractComposition extends AbstractCompositionRule {

	public final static String COMPOSITION_RULE_NAME = "ContractComposition";

	protected static final String PLAIN_CONTRACTING = "plain_contracting";
	protected static final String CONSECUTIVE_CONTRACTING = "consecutive_contracting";
	protected static final String EXPLICIT_CONTRACTING = "explicit_contracting";
	protected static final String CONTRACT_OVERRIDING = "contract_overriding";
	protected static final String CUMULATIVE_CONTRACTING = "cumulative_contracting";
	protected static final String CONJUNCTIVE_CONTRACTING = "conjunctive_contracting";
	protected static final String METHOD_BASED_COMPOSITION = "method_based";

	protected static final String ORIGINAL_KEYWORD_CLAUSE = "\\original_clause";
	protected static final String ORIGINAL_SPEC_KEYWORD = "\\original_spec";
	protected static final String ORIGINAL_CASE_KEYWORD = "\\original_case";
	protected static final String ORIGINAL_KEYWORD = "\\original";
	protected static final String ORIGINAL_OR = "\\or_original";

	protected String contractStyle = PLAIN_CONTRACTING;
	private ContractReader contractReader;

	public ContractComposition(String contract_style) {
		if (contract_style != null)
			contractStyle = contract_style.trim();

	}

	@Override
	public void compose(FSTTerminal terminalA, FSTTerminal terminalB,
			FSTTerminal terminalComp, FSTNonTerminal nonterminalParent)
			throws CompositionException {
		// Check Composition style
		if (contractStyle.equals(PLAIN_CONTRACTING)) {
			plainContracting(terminalA, terminalB, terminalComp);
		} else if (contractStyle.equals(CONTRACT_OVERRIDING)) {
			contractOverriding(terminalA, terminalB, terminalComp);
		} else if (contractStyle.equals(EXPLICIT_CONTRACTING)) {
			explicitContracting(terminalA, terminalB, terminalComp);
		} else if (contractStyle.equals(CONSECUTIVE_CONTRACTING)) {
			consecutiveContracting(terminalA, terminalB, terminalComp);
		} else if (contractStyle.equals(CUMULATIVE_CONTRACTING)) {
			cumulativeContracting(terminalA, terminalB, terminalComp);
		} else if (contractStyle.equals(CONJUNCTIVE_CONTRACTING)) {
			conjunctiveContracting(terminalA, terminalB, terminalComp);
		} else if (contractStyle.equals(METHOD_BASED_COMPOSITION)) {
			compositionByKeywords(terminalA, terminalB, terminalComp,
					nonterminalParent);
		}

		// Does the composition contains non jml keywords?
		if (checkContainsOriginal(terminalComp))
			throw new CompositionException(
					terminalA,
					terminalB,
					"Contract still contains the keyword \\original, \\original_case or \\original_spec after composition!");
	}

	// Check Keywords in Method-Based Contract Composition
	protected void compositionByKeywords(FSTTerminal terminalA,
			FSTTerminal terminalB, FSTTerminal terminalComp,
			FSTNonTerminal nonterminalParent) {
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

	protected void plainContracting(FSTTerminal terminalA, FSTTerminal terminalB,
			FSTTerminal terminalComp) {
		terminalComp.setBody(terminalB.getBody());
	}

	protected void contractOverriding(FSTTerminal terminalA,
			FSTTerminal terminalB, FSTTerminal terminalComp) {
		terminalComp.setBody(terminalA.getBody());
	}

	protected void explicitContracting(FSTTerminal terminalA,
			FSTTerminal terminalB, FSTTerminal terminalComp) {
		terminalComp.setBody(getReplacementString(terminalA, terminalB));
	}

	protected void consecutiveContracting(FSTTerminal terminalA,
			FSTTerminal terminalB, FSTTerminal terminalComp) {
		terminalComp.setBody(terminalB.getBody() + "\n\talso\n\t "
				+ terminalA.getBody());
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

		terminalComp.setBody(joinClauses(reqClaB, reqClaA, "requires", "&&")
				+ "\n\t " + joinClauses(ensClaB, ensClaA, "ensures", "&&"));
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

		terminalComp.setBody(joinClauses(reqClaB, reqClaA, "requires", "||")
				+ "\n\t " + joinClauses(ensClaB, ensClaA, "ensures", "&&"));
	}

	protected void desugarAlso(FSTTerminal terminal) {
		String[] baseCases = terminal.getBody().trim().split("also");

		StringBuilder reqBuilder = new StringBuilder("requires ");
		StringBuilder ensBuilder = new StringBuilder("ensures ");
		for (String ca : baseCases) {
			FSTTerminal temp = new FSTTerminal(terminal.getType(),
					terminal.getName(), ca, "");
			List<FSTTerminal> reqCla = getRequiresClauses(temp);
			List<FSTTerminal> ensCla = getEnsuresClauses(temp);

			if (reqCla.size() > 0 && ensCla.size() > 0) {
				String reqTemp = joinClause(reqCla, "requires");
				reqBuilder.append(reqTemp).append(" || ");
				ensBuilder.append("(\\old").append(reqTemp)
						.append("\n\t\t ==> ")
						.append(joinClause(getEnsuresClauses(temp), "ensures"))
						.append(") && ");

			} else if (reqCla.size() > 0) {
				reqBuilder.append(joinClause(reqCla, "requires"))
						.append(" || ");
			} else if (ensCla.size() > 0) {
				ensBuilder.append(
						joinClause(getEnsuresClauses(temp), "ensures")).append(
						" && ");
			}
		}
		reqBuilder.replace(reqBuilder.lastIndexOf(" || "),
				reqBuilder.lastIndexOf(" || ") + 4, ";");
		ensBuilder.replace(ensBuilder.lastIndexOf(" && "),
				ensBuilder.lastIndexOf(" && ") + 4, ";");
		terminal.setBody(reqBuilder.toString() + "\n\t" + ensBuilder.toString());
	}

	// joins Either Requires or Ensures clauses (claustaype)
	// joins them using the operationtype (&& or ||)
	protected String joinClauses(List<FSTTerminal> reqOrEnsClaB,
			List<FSTTerminal> reqOrEnsClaA, String clauseType,
			String operationType) {
		operationType = "\n\t\t " + operationType + " ";
		StringBuilder builder = new StringBuilder("");

		if (reqOrEnsClaB.size() > 0 && reqOrEnsClaA.size() > 0) {
			builder.append(clauseType).append(" ")
					.append(joinClause(reqOrEnsClaB, clauseType))
					.append(operationType)
					.append(joinClause(reqOrEnsClaA, clauseType)).append(";");
		} else if (reqOrEnsClaB.size() > 0) {
			builder.append(clauseType).append(" ")
					.append(joinClause(reqOrEnsClaB, clauseType)).append(";");
		} else if (reqOrEnsClaA.size() > 0) {
			builder.append(clauseType).append(" ")
					.append(joinClause(reqOrEnsClaA, clauseType)).append(";");
		}

		return builder.toString();
	}

	// Joins all Requires or Ensures clauses with an AND Operator of one
	// contract
	protected String joinClause(List<FSTTerminal> clauses, String clauseType) {
		StringBuilder builder = new StringBuilder("");
		String operationType = " && ";

		for (FSTTerminal cl : clauses)
			builder.append("(")
					.append(cl.getBody()
							.substring(0, cl.getBody().lastIndexOf(";"))
							.replace(clauseType + " ", "")).append(")")
					.append(operationType);
		builder.replace(builder.lastIndexOf(operationType),
				builder.lastIndexOf(operationType) + 4, "");
		if (clauses.size() > 1) {
			builder.insert(0, "(");
			builder.append(")");
		}
		return builder.toString();
	}

	public boolean checkContainsOriginal(FSTTerminal terminal) {
		String body = terminal.getBody();
		return (body.contains(ORIGINAL_CASE_KEYWORD)
				|| body.contains(ORIGINAL_SPEC_KEYWORD) || body
					.contains(ORIGINAL_KEYWORD));

	}

	protected List<FSTTerminal> getRequiresClauses(FSTTerminal terminal) {
		contractReader = new ContractReader(terminal);
		return contractReader.getRequiresClauses();
	}

	protected List<FSTTerminal> getEnsuresClauses(FSTTerminal terminal) {
		contractReader = new ContractReader(terminal);
		return contractReader.getEnsuresClauses();
	}

	protected String getReplacementString(FSTTerminal terminalA,
			FSTTerminal terminalB) {

		terminalA.setBody(terminalA.getBody().trim());
		terminalB.setBody(terminalB.getBody().trim());
		boolean isExtendingSpec = false;

		if (terminalA.getParent().getParent().getType().equals("ExtendingSpec"))
			isExtendingSpec = true;

		String[] baseCases = terminalB.getBody().trim().split("also");

		StringBuffer result = new StringBuffer();
		if (isExtendingSpec)
			result.append("also\n");
		result.append("\t");

		String[] casesA = terminalA.getBody().trim().split("also");

		for (int j = 0; j < casesA.length; j++) {

			String[] clausesA = casesA[j].trim().split(";");

			for (int i = 0; i < clausesA.length; i++) {
				if (!clausesA[i].trim().equals("")) {
					clausesA[i] = clausesA[i] + ";";
				}
			}
			for (int i = 0; i < clausesA.length; i++) {
				if (clausesA[i].contains(ORIGINAL_KEYWORD)
						|| clausesA[i].contains(ORIGINAL_KEYWORD_CLAUSE)
						|| clausesA[i].contains(ORIGINAL_CASE_KEYWORD)
						|| clausesA[i].contains(ORIGINAL_SPEC_KEYWORD)) {
					result.append(replaceOriginal(
							baseCases,
							clausesA[i],
							j,
							clausesA[i].replaceAll("@", "").trim().split(" ")[0])
							.replace(";;", ";"));
				} else {
					// no original in this clause
					result.append(clausesA[i]);
				}
			}

			if (j < casesA.length - 1) {
				result.append("\n\t also\n\t");
			}

		}

		return result.toString().trim();
	}

	protected String replaceOriginal(String[] baseCases, String string,
			int caseId, String prefix) {
		String orig_repl = getOriginalReplacement(baseCases, caseId, prefix);
		if (orig_repl.isEmpty()) {
			// case: no original clause
			// TODO @Fabian is das richtig
			orig_repl = "true";
		}
		String orig_case_repl = getOriginalCaseReplacement(baseCases, caseId);
		String orig_spec_repl = getOriginalSpecReplacement(baseCases);
		return string.replace(ORIGINAL_KEYWORD_CLAUSE, orig_repl)
				.replace(ORIGINAL_KEYWORD, orig_repl)
				.replace(ORIGINAL_SPEC_KEYWORD, orig_spec_repl)
				.replace(ORIGINAL_CASE_KEYWORD, orig_case_repl);
	}

	protected String getOriginalSpecReplacement(String[] baseCases) {
		StringBuffer buf = new StringBuffer();
		boolean append = false;
		for (String s : baseCases) {
			buf.append(s);
			buf.append("\talso");
			append = true;
		}
		if (append)
			buf.setLength(buf.length() - 5);
		return buf.toString();
	}

	protected String getOriginalCaseReplacement(String[] baseCases, int caseId) {

		return baseCases[caseId];
	}

	protected String getOriginalReplacement(String[] baseCases, int caseId,
			String prefix) {

		StringBuffer result = new StringBuffer();
		if (caseId >= baseCases.length)
			throw new RuntimeException(
					"Original() reference cannot be satisfied, specification case: # "
							+ caseId);

		String[] prefixes = new String[baseCases.length];
		for (int i = 0; i < baseCases.length; i++) {

			prefixes[i] = prefixes[i] + "behavior ";

			String baseCase = baseCases[i].replaceAll("@", "").trim();

			if (baseCase.startsWith("public ")) {
				prefixes[i] = "public ";
				baseCase = baseCase.substring(7);
			} else if (baseCase.startsWith("private ")) {
				prefixes[i] = "private ";
				baseCase = baseCase.substring(8);
			} else if (baseCase.startsWith("protected ")) {
				prefixes[i] = "protected ";
				baseCase = baseCase.substring(10);
			}

			if (baseCase.startsWith("behavior ")) {
				prefixes[i] = prefixes[i] + "behavior ";
				baseCase = baseCase.substring(9);
			} else if (baseCase.startsWith("normal_behavior ")) {
				prefixes[i] = prefixes[i] + "normal_behavior  ";
				baseCase = baseCase.substring(16);
			} else if (baseCase.startsWith("exceptional_behavior ")) {
				prefixes[i] = prefixes[i] + "exceptional_behavior  ";
				baseCase = baseCase.substring(21);
			}

			baseCases[caseId] = baseCase;
		}

		FSTGenComposer.outStream.println("baseCases(id)= " + baseCases[caseId]);
		Pattern p = Pattern
				.compile(
						".*\\(\\\\(forall|exists|max|min|num_of|product|sum)[^;]*(;)[^;]*(;).*\\).*",
						Pattern.DOTALL);

		Matcher m = p.matcher(baseCases[caseId]);

		while (m.find()) {
			for (int i = 2; i <= m.groupCount(); i++) {
				StringBuilder sb = new StringBuilder(baseCases[caseId]);
				sb.setCharAt(m.start(i), '#');
				baseCases[caseId] = sb.toString();
			}
			m = p.matcher(baseCases[caseId]);
			FSTGenComposer.outStream.println("XX " + baseCases[caseId]);
		}
		String[] clausesA = baseCases[caseId].trim().split(";");

		boolean append = false;
		for (int i = 0; i < clausesA.length; i++) {

			if (clausesA[i].trim().startsWith(prefix + " ")) {
				result.append("(" + clausesA[i].trim().replaceFirst(prefix, "")
						+ " )");

				result.append(" && ");
				append = true;
			}

		}
		if (append)
			result.setLength(result.length() - 4);

		return result.toString().replaceAll("#", ";");
	}

	@Override
	public String getRuleName() {
		return COMPOSITION_RULE_NAME;
	}

}
