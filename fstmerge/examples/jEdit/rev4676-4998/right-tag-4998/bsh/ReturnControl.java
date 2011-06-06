


package bsh;


class ReturnControl implements ParserConstants {
	public int kind;
	public Object value;

	public ReturnControl( int kind, Object value ) {
		this.kind = kind;
		this.value = value;
	}
}

