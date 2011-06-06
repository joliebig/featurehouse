


package bsh;


public class UtilEvalError extends Exception 
{
	protected UtilEvalError() {
	}

	public UtilEvalError( String s ) {
		super(s);
	}

	
	public EvalError toEvalError( 
		String msg, SimpleNode node, CallStack callstack  ) 
	{
		if ( Interpreter.DEBUG )
			printStackTrace();

		if ( msg == null )
			msg = "";
		else
			msg = msg + ": ";
		return new EvalError( msg+getMessage(), node, callstack );
	}

	public EvalError toEvalError ( SimpleNode node, CallStack callstack ) 
	{
		return toEvalError( null, node, callstack );
	}

}

