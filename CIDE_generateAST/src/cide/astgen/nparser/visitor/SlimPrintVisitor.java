package cide.astgen.nparser.visitor;

import java.io.PrintStream;

import cide.astgen.nparser.ast.NAbstractValue;
import cide.astgen.nparser.ast.NChoice;
import cide.astgen.nparser.ast.NNonTerminal;
import cide.astgen.nparser.ast.NProduction;
import cide.astgen.nparser.ast.NTextOnly;
import cide.astgen.nparser.ast.NValue;

public class SlimPrintVisitor extends NVisitor {

	private PrintStream out;

	public SlimPrintVisitor(PrintStream out) {
		this.out = out;
	}

	@Override
	public boolean visit(NProduction p) {
		out.print(p.getName() + " : ");
		return super.visit(p);
	}

	@Override
	public boolean visit(NChoice c) {
		out.print("\n    ");
		return super.visit(c);
	}

	@Override
	public boolean visit(NNonTerminal t) {
		visitA(t);
		return super.visit(t);
	}

	@Override
	public boolean visit(NValue t) {
		visitA(t);
		return super.visit(t);
	}

	public boolean visit(NTextOnly t) {
		visitA(t);
		return super.visit(t);
	}

	public boolean visitA(NAbstractValue t) {
		for (String x : t.outerPreTokens)
			out.print(x + " ");
		switch (t.type) {
		case ZEROORONE:
			out.print("[");
			printInner(t);
			out.print("]");
			break;
		case ZEROORMORE:
			out.print("(");
			printInner(t);
			out.print(")*");
			break;
		case ONEORMORE:
			out.print("(");
			printInner(t);
			out.print(")+");
			break;
		case ONE:
			printInner(t);
		}
		out.print(" ");
		for (String x : t.outerPostTokens)
			out.print(x + " ");
		return true;
	}

	private void printInner(NAbstractValue t) {
		for (String x : t.innerPreTokens)
			out.print(x + " ");
		if (t instanceof NTextOnly)
			out.print("<NONE>");
		else
			out.print(t.getName());
		for (String x : t.innerPostTokens)
			out.print(" " + x);
	}

	@Override
	public void postVisit(NChoice g) {
		if (g.getParent().getChoices().indexOf(g) != g.getParent().getChoices()
				.size() - 1)
			out.print(" |");
		super.postVisit(g);
	}

	public void postVisit(NProduction g) {
		out.println(";");
		out.println();
		super.postVisit(g);
	}

}
