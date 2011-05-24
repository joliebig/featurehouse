package cide.astgen.nparser.ast;

import cide.astgen.nparser.visitor.NVisitor;

public class NNonTerminal extends NAbstractValue {

	private String origNTerminal;
	/**
	 * used only if the target production should be used as a wrapper. in this
	 * case it stores the type of the wrap target (wrappee is specified inside
	 * the target production).
	 */
	private String wrapsAroundType = null;

	public NNonTerminal(NChoice parent, Type type, String terminal) {
		super(parent, terminal.toString(), type);
		this.origNTerminal = terminal;
	}

	public void accept(NVisitor visitor) {
		visitor.visit(this);
		visitor.postVisit(this);
	}

	@Override
	public String genVariablePlainType() {
		if (wrapsAroundType != null)
			return "ASTNode";

		return name;
	}

	public boolean isWrapper() {
		return wrapsAroundType != null && type == Type.ONE;
	}

	public String getWrapsAroundType() {
		return wrapsAroundType;
	}

	public void setWrapsAroundType(String type) {
		wrapsAroundType = type;
	}

	/**
	 * searches the grammar for the target production. searches inside this
	 * target production for a property that has been marked as wrappee.
	 * 
	 * @return name of the wrappee property in the target production or null if
	 *         not found.
	 */
	public String getWrappeePropertyName() {
		NGrammar grammar = parent.production.getGrammar();
		NProduction production = grammar.findProduction(origNTerminal);
		if (production == null)
			return null;
		if (production.choices.size() != 1)
			throw new UnsupportedOperationException(
					"Cannot wrap around production with multiple choices.");
		for (NAbstractValue unit : production.choices.get(0).getUnits())
			if (unit.isWrappee())
				return unit.genPropertyName();
		return null;
	}

	@Override
	public NAbstractValue cloneValue() {
		return new NNonTerminal(parent, type, origNTerminal);
	}

	@Override
	protected void adjustFrom(NAbstractValue template) {
		super.adjustFrom(template);
		this.wrapsAroundType = ((NNonTerminal) template).wrapsAroundType;
	}
}
