package de.ovgu.cide.fstgen;

import java.util.List;

import cide.astgen.nparser.ast.NAbstractValue;
import cide.astgen.nparser.ast.NChoice;
import cide.astgen.nparser.ast.NNonTerminal;
import cide.astgen.nparser.ast.NProduction;
import cide.astgen.nparser.visitor.NVisitor;

public class FSTInlineVisitor extends NVisitor {

	private boolean isError = false;
	private String errorMsg = "";

	public String getErrors() {
		return errorMsg;
	}

	public boolean hasErrors() {
		return isError;
	}

	@Override
	public boolean visit(NChoice c) {
		boolean isNonTerminal = c.findAnnotation("FSTNonTerminal") != null;
		if (isNonTerminal) {
			for (int idx = c.units.size() - 1; idx >= 0; idx--) {
				if (c.units.get(idx) instanceof NNonTerminal) {
					NProduction referencedProduction = c.getParent()
							.getGrammar().findProduction(
									c.units.get(idx).getName());
					boolean shouldInline = referencedProduction != null
							&& referencedProduction.getChoices().size() == 1;
					if (shouldInline)
						shouldInline &= referencedProduction.getChoices()
								.get(0).findAnnotation("FSTInline") != null;

					if (shouldInline) {
						c.units.remove(idx);
						List<NAbstractValue> units = referencedProduction
								.getChoices().get(0).getUnits();
						c.units.addAll(idx, units);
						idx += units.size();
					}

				}

			}

		}

		return super.visit(c);
	}
}
