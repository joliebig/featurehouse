package processor.capprox;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;
import de.ovgu.cide.fstgen.ast.FSTVisitor;

public class CForwardDeclarationOnTop extends FSTVisitor {

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
