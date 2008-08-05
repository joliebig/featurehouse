package cide.gast;

import java.util.ArrayList;

public class ASTStringNode extends ASTNode {
	private String value;

	public ASTStringNode(String value, IToken token) {
		super(new ArrayList<Property>(), token, token);
		this.value = value;
	}

	public String getValue() {
		return value;
	}

	public String toString() {
		return value;
	}

	@Override
	public ASTNode deepCopy() {
		return new ASTStringNode(new String(value), firstToken);
	}
}
