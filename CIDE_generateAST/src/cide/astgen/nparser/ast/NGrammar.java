package cide.astgen.nparser.ast;

import java.util.ArrayList;
import java.util.List;

import cide.astgen.nparser.visitor.NVisitor;

public class NGrammar {
	final public List<NProduction> productions;
	private String introduction;

	public NGrammar(List<NProduction> productions) {
		this.productions = productions;
	}

	public NGrammar(String introduction) {
		this(new ArrayList<NProduction>());
		this.introduction = introduction;
	}

	public void accept(NVisitor visitor) {
		if (visitor.visit(this))
			for (NProduction p : productions)
				p.accept(visitor);
		visitor.postVisit(this);
	}

	public NProduction findProduction(String name) {
		for (NProduction production : productions)
			if (production.getName().equals(name))
				return production;
		return null;
	}

	public String getIntroduction() {
		return introduction;
	}

	@Override
	public String toString() {
		String r = "";
		for (NProduction k : productions)
			r += k.toString()+"\n\n";
		return r;
	}
}
