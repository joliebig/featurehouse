package cide.astgen.nparser.ast;

import cide.astgen.nparser.visitor.NVisitor;

/**
 * element of a choice, that only consists of one or more text elements but no
 * value or non-terminal
 * 
 * @author cKaestner
 * 
 */
public class NTextOnly extends NAbstractValue {

	private static int generateIdx = 0;

	public NTextOnly(NChoice parent, Type type) {
		super(parent, generateName(), type);
	}

	private static String generateName() {
		return "text" + (++generateIdx);
	}

	@Override
	public void accept(NVisitor visitor) {
		visitor.visit(this);
		visitor.postVisit(this);
	}

	@Override
	public String genVariablePlainType() {
		return "ASTTextNode";
	}

	@Override
	public NAbstractValue cloneValue() {
		return new NTextOnly(parent, type);
	}
}
