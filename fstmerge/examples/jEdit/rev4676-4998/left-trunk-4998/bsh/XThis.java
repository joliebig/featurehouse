


package bsh;

import java.lang.reflect.*;
import java.lang.reflect.InvocationHandler;
import java.io.*;
import java.util.Hashtable;


class XThis extends This 
	{
	
	Hashtable interfaces;

	InvocationHandler invocationHandler = new Handler();

	XThis( NameSpace namespace, Interpreter declaringInterp ) { 
		super( namespace, declaringInterp ); 
	}

	public String toString() {
		return "'this' reference (XThis) to Bsh object: " + namespace;
	}

	
	public Object getInterface( Class clas ) 
	{
		return getInterface( new Class[] { clas } );
	}

	
	public Object getInterface( Class [] ca ) 
	{
		if ( interfaces == null )
			interfaces = new Hashtable();

		
		int hash = 21;
		for(int i=0; i<ca.length; i++)
			hash *= ca[i].hashCode() + 3;
		Object hashKey = new Integer(hash);

		Object interf = interfaces.get( hashKey );

		if ( interf == null ) 
		{
			ClassLoader classLoader = ca[0].getClassLoader(); 
			interf = Proxy.newProxyInstance( 
				classLoader, ca, invocationHandler );
			interfaces.put( hashKey, interf );
		}

		return interf;
	}

	
	class Handler implements InvocationHandler, java.io.Serializable 
	{
		public Object invoke( Object proxy, Method method, Object[] args ) 
			throws Throwable
		{
			try { 
				return invokeImpl( proxy, method, args );
			} catch ( TargetError te ) {
				
				
				
				throw te.getTarget();
			} catch ( EvalError ee ) {
				
				
				if ( Interpreter.DEBUG ) 
					Interpreter.debug( "EvalError in scripted interface: "
					+ XThis.this.toString() + ": "+ ee );
				throw ee;
			}
		}

		public Object invokeImpl( Object proxy, Method method, Object[] args ) 
			throws EvalError 
		{
			String methodName = method.getName();
			CallStack callstack = new CallStack( namespace );

			
			BshMethod equalsMethod = null;
			try {
				equalsMethod = namespace.getMethod( 
					"equals", new Class [] { Object.class } );
			} catch ( UtilEvalError e ) { }
			if ( methodName.equals("equals" ) && equalsMethod == null ) {
				Object obj = args[0];
				return new Boolean( proxy == obj );
			}

			
			BshMethod toStringMethod = null;
			try {
				toStringMethod = 
					namespace.getMethod( "toString", new Class [] { } );
			} catch ( UtilEvalError e ) { }

			if ( methodName.equals("toString" ) && toStringMethod == null)
			{
				Class [] ints = proxy.getClass().getInterfaces();
				
				StringBuffer sb = new StringBuffer( 
					XThis.this.toString() + "\nimplements:" );
				for(int i=0; i<ints.length; i++)
					sb.append( " "+ ints[i].getName() 
						+ ((ints.length > 1)?",":"") );
				return sb.toString();
			}

			Class [] paramTypes = method.getParameterTypes();
			return Primitive.unwrap( 
				invokeMethod( methodName, Primitive.wrap(args, paramTypes) ) );
		}
	};
}



