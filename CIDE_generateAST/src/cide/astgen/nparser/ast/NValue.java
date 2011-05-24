package cide.astgen.nparser.ast;

import cide.astgen.nparser.visitor.NVisitor;

public class NValue extends NAbstractValue {

	public NValue(NChoice parent, Type type, String name) {
		super(parent, name, type);
	}

	public void accept(NVisitor visitor) {
		visitor.visit(this);
		visitor.postVisit(this);
	}

	@Override
	public String getName() {

		return "<" + super.getName() + ">";
	}

	@Override
	public String genVariablePlainName() {
		return super.genVariablePlainName().toLowerCase();
	}

	public String genVariablePlainType() {
		return "ASTStringNode";
	}

	@Override
	public NAbstractValue cloneValue() {
		return new NValue(parent, type, name);
	}


}