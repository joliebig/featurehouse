


package bsh;


public class EvalError extends Exception {
	
	SimpleNode node;

	
	String message;

	public EvalError(String s) {
		setMessage(s);
	}

	public EvalError(String s, SimpleNode node) {
		this(s);
		this.node = node;
	}

	
	public String toString() {
		String trace;
		if ( node != null )
			trace = " : at Line: "+ node.getLineNumber() 
				+ " : in file: "+ node.getSourceFile()
				+ " : "+node.getText();
		else
			
			
			trace = ": <at unknown location>";

			
			return getMessage() + trace;
	}

	
	public void reThrow( String msg ) 
		throws EvalError 
	{
		reThrow( msg, null );
	}

	
	public void reThrow( SimpleNode node ) 
		throws EvalError 
	{
		reThrow( null, node );
	}

	
	public void reThrow( String addMsg, SimpleNode addNode ) 
		throws EvalError 
	{
		prependMessage( addMsg );
		addNode( addNode );
		throw this;
	}

	
	void setNode( SimpleNode node ) {
		this.node = node;
	}

	
	SimpleNode getNode() {
		return node;
	}

	public String getErrorText() { 
		if ( node != null )
			return node.getText() ;
		else
			return "<unknown error>";
	}

	public int getErrorLineNumber() { 
		if ( node != null )
			return node.getLineNumber() ;
		else
			return -1;
	}

	public String getErrorSourceFile() {
		if ( node != null )
			return node.getSourceFile() ;
		else
			return "<unknown file>";
	}

	public String getMessage() { return message; }

	public void setMessage( String s ) { message = s; }

	
	protected void prependMessage( String s ) { 
		if ( s != null )
			message = s + " : "+ message;
	}

	protected void addNode( SimpleNode addNode  ) {
		SimpleNode node = this.node;
		if ( node == null && addNode != null )
			node = addNode;
	}
}

