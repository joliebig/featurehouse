


package org.gjt.sp.jedit.bsh;


public class UtilTargetError extends UtilEvalError
{
	public Throwable t;

	public UtilTargetError( String message, Throwable t ) {
		super( message );
		this.t = t;
	}

	public UtilTargetError( Throwable t ) {
		this( null, t );
	}

	
	public EvalError toEvalError( 
		String msg, SimpleNode node, CallStack callstack  ) 
	{
		if ( msg == null )
			msg = getMessage();
		else
			msg = msg + ": " + getMessage();

		return new TargetError( msg, t, node, callstack, false );
	}
}

