package cide.gparser;

import cide.gast.IToken;

public class WToken implements IToken {

	private int offset, length;

	public WToken(Token token) {
		offset = token.offset;
		length = token.length;
//		if (token.image==null || token.image.length()==0) length=0;
		image = token.image;
	}

	private String image;

	@Override
	public String toString() {
		return "<" + offset + " - " + (offset + length) + " -> " + image + ">";
	}

	public int getLength() {
		return length;
	}

	public int getOffset() {
		return offset;
	}

}
