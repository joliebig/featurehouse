package composer.rules;

import composer.CompositionException;

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
			FSTTerminal terminalComp, FSTNonTerminal nonterminalParent)
			throws CompositionException {
		String keywordA = terminalA.getContractCompKey();
		String keywordB = terminalB.getContractCompKey();

			if (METHOD_BASED_COMPOSITION.equals(contract_style)) {
				if (keywordB.equals("\\final_contract")
						&& !keywordB.equals("\\final_method")) {
					throw new CompositionException(
							null,
							terminalA,
							"Contracts which contain the keyword \\final_contract can only be refined with the keyword \\final_method!");
				} else if(keywordB.equals("\\final_method")) {
					throw new CompositionException(
							null,
							terminalA,
							"It is not allowed to refine a contract which contains the keyword \\final_method!");
				}
				
				if (keywordA.equals("")) {
					terminalComp.setContractCompKey(keywordB);
				} else if (keywordB.equals("")
						|| isValidOrder(keywordA, keywordB)) {
					terminalComp.setContractCompKey(keywordA);
				} else if (!isValidOrder(keywordA, keywordB)) {
					terminalComp.setContractCompKey(keywordB);
					throw new CompositionException(null, terminalA,
							"Overriding Keyword " + keywordB + " with "
									+ keywordA + " is not allowed!");
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
