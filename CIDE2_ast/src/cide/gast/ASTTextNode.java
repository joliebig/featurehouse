package cide.gast;

import java.util.ArrayList;

public class ASTTextNode extends ASTNode {
	private String[] value;

	public ASTTextNode(String value, IToken token) {
		super(new ArrayList<Property>(), token, token);
		this.value = new String[] { value };
	}

	public ASTTextNode(String[] value, IToken token) {
		super(new ArrayList<Property>(), token, token);
		this.value = value;
	}

	public String getValue() {
		String result = "";
		for (int idx = 0; idx < value.length; idx++) {
			if (idx != 0)
				result += ",";
			result += value[idx];
		}
		return result;
	}

	public String toString() {
		return getValue();
	}

	@Override
	public ASTNode deepCopy() {
		return new ASTTextNode(value, firstToken);
	}
}
