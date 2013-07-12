package composer.rules;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class ContractKeywordComposition extends AbstractCompositionRule {
	private static final String METHOD_BASED_COMPOSITION = "method_based";
	private String contract_style = "none";

	public ContractKeywordComposition(String contract_style) {
		this.contract_style = contract_style;
	}

	@Override
	public void compose(FSTTerminal terminalA, FSTTerminal terminalB,
			FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) {
		String keywordA = terminalA.getContractCompKey();
		String keywordB = terminalB.getContractCompKey();

		if (METHOD_BASED_COMPOSITION.equals(contract_style)) {
			if (keywordA.equals("")) {
				terminalComp.setContractCompKey(keywordB);
			} else if (keywordB.equals("") || isValidOrder(keywordA, keywordB)) {
				terminalComp.setContractCompKey(keywordA);
			} else if (!isValidOrder(keywordA, keywordB)) {
				// TODO throw Composition Exception
				System.out.println("Overriding Keyword " + keywordB + " with "
						+ keywordA + " is not allowed!");
				terminalComp.setContractCompKey(keywordB);
			}
		}
		
		terminalComp.setBody("\n\t");
	}

	// test, if key overriding is allowed. (rank order)
	private boolean isValidOrder(String keywordA, String keywordB) {
		CompositionKeyword CompKeyA = CompositionKeyword
				.getCompositionKeyword(keywordA);
		CompositionKeyword CompKeyB = CompositionKeyword
				.getCompositionKeyword(keywordB);

		return CompKeyA.getRank() <= CompKeyB.getRank();
	}

}
