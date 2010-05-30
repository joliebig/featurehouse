package processor.capprox;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;
import de.ovgu.cide.fstgen.ast.FSTVisitor;

public class CForwardDeclarationGenerator extends FSTVisitor {

    public boolean visit(FSTTerminal terminal) {
	String s = "(?m)(?s).*\\}.*";
	// System.out.println(terminal.getBody());
	// System.out.println(s);
	if (terminal.getType() == "Func")
	    if (terminal.getBody().matches(s)) {
		System.out.println(terminal);
		
		FSTTerminal forwardDeclaration = new FSTTerminal("Func",
			terminal.getName(), terminal.getBody().replaceAll(
				"(?s)(?m)\\{.*", ";"), "", terminal
				.getCompositionMechanism());
		System.out.println(forwardDeclaration);
		
		 ((FSTNonTerminal) terminal.getParent())
		 .addChild(forwardDeclaration);
	    }
	return true;
    }

    public boolean visit(FSTNonTerminal nonTerminal) {
	if (nonTerminal.getType() == "Feature")
	    System.out.println(nonTerminal);
	return true;
    }

    public void postVisit(FSTTerminal terminal) {
    }

    public void postVisit(FSTNonTerminal nonTerminal) {
    }
}
