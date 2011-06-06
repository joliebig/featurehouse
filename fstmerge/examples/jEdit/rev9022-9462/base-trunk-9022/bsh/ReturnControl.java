


package bsh;


class ReturnControl implements ParserConstants {
	public int kind;
	public Object value;
	
	public SimpleNode returnPoint;

	public ReturnControl( int kind, Object value, SimpleNode returnPoint ) {
		this.kind = kind;
		this.value = value;
		this.returnPoint = returnPoint;
	}
}

