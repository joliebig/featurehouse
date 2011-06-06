


package bsh;

import java.io.IOException;


public class This implements java.io.Serializable, Runnable {
	
	NameSpace namespace;

	
	transient Interpreter declaringInterpreter;

	

	

	
    static This getThis( 
		NameSpace namespace, Interpreter declaringInterpreter ) 
	{
		try {
			if ( Capabilities.canGenerateInterfaces() )
				return (This)Reflect.constructObject( "bsh.XThis",
					new Object [] { namespace, declaringInterpreter } );
			else if ( Capabilities.haveSwing() )
				return (This)Reflect.constructObject( "bsh.JThis",
					new Object [] { namespace, declaringInterpreter } );
			else
				return new This( namespace, declaringInterpreter );

		} catch ( Exception e ) {
			throw new InterpreterError("internal error 1 in This: "+e);
		} 
    }

	
	public Object getInterface( Class clas ) 
		throws EvalError
	{
		if ( clas.isInstance( this ) )
			return this;
		else
			throw new EvalError( "Dynamic proxy mechanism not available. "
			+ "Cannot construct interface type: "+clas );
	}

	
	protected This( NameSpace namespace, Interpreter declaringInterpreter ) { 
		this.namespace = namespace; 
		this.declaringInterpreter = declaringInterpreter;
		
	}

	public NameSpace getNameSpace() {
		return namespace;
	}

	public String toString() {
		return "'this' reference to Bsh object: " + namespace.name;
	}

	public void run() {
		try {
			invokeMethod( "run", new Object[0] );
		} catch( EvalError e ) {
			declaringInterpreter.error(
				"Exception in runnable:" + e );
		}
	}

	
	public Object invokeMethod( String name, Object [] args ) 
		throws EvalError
	{
		
		
		return invokeMethod( name, args, declaringInterpreter, null, null );
	}

	
	public Object invokeMethod( 
		String name, Object [] args, Interpreter interpreter, 
			CallStack callstack, SimpleNode callerInfo  ) 
		throws EvalError
	{
		return namespace.invokeMethod( 
			name, args, interpreter, callstack, callerInfo );
	}


	
	public static void bind( 
		This ths, NameSpace namespace, Interpreter declaringInterpreter ) 
	{ 
		ths.namespace.setParent( namespace ); 
		ths.declaringInterpreter = declaringInterpreter;
		
	}

	


	CallStack newCallStack() {
		CallStack callstack = new CallStack();
		callstack.push( namespace );
		return callstack;
	}
}

