package de.ovgu.cide.fstgen.ast;

public class FSTVisitor {

	public boolean visit(FSTTerminal terminal) {
		return true;
	}

	public boolean visit(FSTNonTerminal nonTerminal) {
		return true;
	}

	public void postVisit(FSTTerminal terminal) {
	}

	public void postVisit(FSTNonTerminal nonTerminal) {
	}

}
