package printer;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public interface PrintVisitorInterface {
	public abstract void visit(FSTNonTerminal nonterminal)  throws PrintVisitorException;
}
