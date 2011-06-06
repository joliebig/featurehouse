


package org.gjt.sp.jedit.bsh;


public class EvalError extends Exception 
{
	SimpleNode node;

	
	String message;

	CallStack callstack;

	public EvalError( String s, SimpleNode node, CallStack callstack ) {
		setMessage(s);
		this.node = node;
		
		if ( callstack != null )
			this.callstack = callstack.copy();
	}

	
	public String toString() 
	{
		String trace;
		if ( node != null )
			trace = " : at Line: "+ node.getLineNumber() 
				+ " : in file: "+ node.getSourceFile()
				+ " : "+node.getText();
		else
			
			trace = ": <at unknown location>";

		if ( callstack != null )
			trace = trace +"\n" + getScriptStackTrace();

		return getMessage() + trace;
	}

	
	public void reThrow( String msg ) 
		throws EvalError 
	{
		prependMessage( msg );
		throw this;
	}

	
	SimpleNode getNode() {
		return node;
	}

	void setNode( SimpleNode node ) {
		this.node = node;
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

	public String getScriptStackTrace() 
	{
		if ( callstack == null )
			return "<Unknown>";

		String trace = "";
		CallStack stack = callstack.copy();
		while ( stack.depth() > 0 ) 
		{
			NameSpace ns = stack.pop();
			SimpleNode node = ns.getNode();
			if ( ns.isMethod )
			{
				trace = trace + "\nCalled from method: " + ns.getName();
				if ( node != null )
					trace += " : at Line: "+ node.getLineNumber() 
						+ " : in file: "+ node.getSourceFile()
						+ " : "+node.getText();
			}
		}

		return trace;
	}

	
	public String getMessage() { return message; }

	public void setMessage( String s ) { message = s; }

	
	protected void prependMessage( String s ) 
	{ 
		if ( s == null )
			return;

		if ( message == null )
			message = s;
		else
			message = s + " : "+ message;
	}

}

