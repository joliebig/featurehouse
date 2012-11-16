package composer.rules;



import java.util.regex.Matcher;
import java.util.regex.Pattern;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class ContractComposition extends AbstractCompositionRule {

	public final static String COMPOSITION_RULE_NAME = "ContractComposition";
	
	private static final String PLAIN_CONTRACTING = "plain_contracting";
	private static final String CONSECUTIVE_CONTRACTING = "consecutive_contracting";
	private static final String EXPLICIT_CONTRACTING = "explicit_contracting";
	private static final String CONTRACT_OVERRIDING = "contract_overriding";
	
	protected static final String ORIGINAL_KEYWORD_CLAUSE = "\\original_clause";
	protected static final String ORIGINAL_SPEC_KEYWORD = "\\original_spec";
	protected static final String ORIGINAL_CASE_KEYWORD = "\\original_case";
	protected static final String ORIGINAL_KEYWORD = "\\original";
	protected static final String ORIGINAL_OR = "\\or_original";
	
	private String contractStyle = PLAIN_CONTRACTING;

	public ContractComposition(String contract_style) {
		if (contract_style != null) {
			contractStyle = contract_style.trim();
		}
	}

	@Override
	public void compose(FSTTerminal terminalA, FSTTerminal terminalB,
			FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) {
		if (contractStyle.equals(PLAIN_CONTRACTING)) {
			terminalComp.setBody(terminalB.getBody());
		} else if (contractStyle.equals(CONTRACT_OVERRIDING)) {
			terminalComp.setBody(terminalA.getBody());
		} else if (contractStyle.equals(EXPLICIT_CONTRACTING)) {
			terminalComp.setBody(getReplacementString(terminalA, terminalB));
		} else if (contractStyle.equals(CONSECUTIVE_CONTRACTING)) {
			terminalComp.setBody(terminalB.getBody() + "\nalso\n"
					+ terminalA.getBody());
		}
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
				if (clausesA[i].contains(ORIGINAL_KEYWORD)|| clausesA[i].contains(ORIGINAL_KEYWORD_CLAUSE)||clausesA[i].contains(ORIGINAL_CASE_KEYWORD)||clausesA[i].contains(ORIGINAL_SPEC_KEYWORD)) {
					result.append(replaceOriginal(baseCases, clausesA[i], j,
							clausesA[i].replaceAll("@", "").trim().split(" ")[0]).replace(";;", ";"));
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


	private String replaceOriginal(String[] baseCases, String string,
			int caseId, String prefix) {
		String orig_repl = getOriginalReplacement(baseCases, caseId, prefix);
		if (orig_repl.isEmpty()) {
			// case: no original clause 
//			TODO @Fabian is das richtig 
			orig_repl = "true";
		}
		String orig_case_repl = getOriginalCaseReplacement(baseCases, caseId);
		String orig_spec_repl = getOriginalSpecReplacement(baseCases);
		return string.replace(ORIGINAL_KEYWORD_CLAUSE, orig_repl)
				.replace(ORIGINAL_KEYWORD, orig_repl)
				.replace(ORIGINAL_SPEC_KEYWORD, orig_spec_repl)
				.replace(ORIGINAL_CASE_KEYWORD, orig_case_repl);
	}

	private String getOriginalSpecReplacement(String[] baseCases) {
			StringBuffer buf = new StringBuffer();
			boolean append = false;
			for(String s:baseCases){
				buf.append(s);
				buf.append("\talso");
				append=true;
			}
			if (append)
				buf.setLength(buf.length() - 5);
			return buf.toString();
		}

	private String getOriginalCaseReplacement(String[] baseCases, int caseId) {
	
		return baseCases[caseId];
	}

	private String getOriginalReplacement(String[] baseCases, int caseId,
			String prefix) {
		
		StringBuffer result = new StringBuffer();
		if (caseId >= baseCases.length)
			throw new RuntimeException(
					"Original() reference cannot be satisfied, specification case: # "
							+ caseId);
	

		String[] prefixes = new String[baseCases.length];
		for(int i=0;i<baseCases.length;i++)
		{
		
		prefixes[i]=prefixes[i]+"behavior ";
		
		String baseCase= baseCases[i].replaceAll("@", "").trim();
		
		if(baseCase.startsWith("public ")){
			prefixes[i]="public ";
			baseCase=baseCase.substring(7);
		}
		else if(baseCase.startsWith("private ")){
			prefixes[i]="private ";
			baseCase=baseCase.substring(8);
		}	
		else if(baseCase.startsWith("protected ")){
			prefixes[i]="protected ";
			baseCase=baseCase.substring(10);
		}
		
		if(baseCase.startsWith("behavior ")){
			prefixes[i]=prefixes[i]+"behavior ";
			baseCase=baseCase.substring(9);
		}
		else if(baseCase.startsWith("normal_behavior ")){
			prefixes[i]=prefixes[i]+"normal_behavior  ";
			baseCase=baseCase.substring(16);
		}	
		else if(baseCase.startsWith("exceptional_behavior ")){
			prefixes[i]=prefixes[i]+"exceptional_behavior  ";
			baseCase=baseCase.substring(21);
		}
		
		baseCases[caseId]=baseCase;
		}
	
	
		System.out.println("baseCases(id)= "+baseCases[caseId]);
		Pattern p = Pattern.compile(".*\\(\\\\(forall|exists|max|min|num_of|product|sum).*(;).*(;).*\\).*",Pattern.DOTALL);
		
		Matcher m = p.matcher(baseCases[caseId]);
		
		while(m.find()){
		for(int i = 2; i<=m.groupCount();i++){
			StringBuilder sb = new StringBuilder(baseCases[caseId]);
			sb.setCharAt(m.start(i), '�');
			baseCases[caseId]=sb.toString();
		}
		System.out.println("XX "+baseCases[caseId]);
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
		
		return result.toString().replaceAll("�", ";");
	}

	@Override
	public String getRuleName() {
		return COMPOSITION_RULE_NAME;
	}
	

}
