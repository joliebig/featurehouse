package cide.astgen.nparser.ast;


public class NJavaToken extends NValue {

	public NJavaToken(NChoice parent, Type type, String name) {
		super(parent, type, name);
	}

	public String getName() {
		return name + "()";
	}
}