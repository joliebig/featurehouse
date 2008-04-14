package cide.astgen.nparser.visitor;

import cide.astgen.nparser.ast.NChoice;
import cide.astgen.nparser.ast.NGrammar;
import cide.astgen.nparser.ast.NNonTerminal;
import cide.astgen.nparser.ast.NProduction;
import cide.astgen.nparser.ast.NTextOnly;
import cide.astgen.nparser.ast.NValue;

public class NVisitor {
	public boolean visit(NGrammar g) {
		return true;
	}

	public boolean visit(NProduction p) {
		return true;
	}

	public boolean visit(NChoice c) {
		return true;
	}

	public boolean visit(NNonTerminal t) {
		return true;
	}

	public boolean visit(NValue t) {
		return true;
	}

	public boolean visit(NTextOnly t) {
		return true;
	}

	public void postVisit(NGrammar g) {
	}

	public void postVisit(NProduction g) {
	}

	public void postVisit(NChoice g) {
	}

	public void postVisit(NNonTerminal g) {
	}

	public void postVisit(NValue g) {
	}

	public void postVisit(NTextOnly g) {
	}
}
