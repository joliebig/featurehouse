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
		String keywordA = terminalA.getBody();
		String keywordB = terminalB.getBody();

		if (METHOD_BASED_COMPOSITION.equals(contract_style)) {
			if (keywordA.equals("")) {
				terminalComp.setBody(keywordB);
			} else if (keywordB.equals("")
					|| isValidOrder(keywordA, keywordB)) {
				terminalComp.setBody(keywordA);
			} else if (!isValidOrder(keywordA, keywordB)) {
				//TODO throw ParseException
				System.out.println("Überschreiben von: " + keywordB + " durch "
						+ keywordA + " nicht erlaubt!");
				terminalComp.setBody(keywordB);
			}
		} else {
			terminalComp.setBody("");
		}
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
