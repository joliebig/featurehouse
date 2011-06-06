


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
		return "'this' reference (XThis) to Bsh object: " + namespace.name;
	}

	String toStringShowInts( Class [] ints ) {
		StringBuffer sb = new StringBuffer( toString() + "\nimplements:" );
		for(int i=0; i<ints.length; i++)
			sb.append( " "+ ints[i].getName() + ((ints.length > 1)?",":"") );
		return sb.toString();
	}

	
	public Object getInterface( Class clas ) {
		if ( interfaces == null )
			interfaces = new Hashtable();

		Object interf = interfaces.get( clas );
		if ( interf == null ) {
			interf = Proxy.newProxyInstance( clas.getClassLoader(), 
				new Class[] { clas }, invocationHandler );
			interfaces.put( clas, interf );
		}
		return interf;
	}

	

	
	class Handler implements InvocationHandler, java.io.Serializable 
	{
		public Object invoke( Object proxy, Method method, Object[] args ) 
			throws EvalError 
		{
			try { 
				return invokeImpl( proxy, method, args );
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
			CallStack callstack = newCallStack();

			Class [] sig = Reflect.getTypes( args );
			BshMethod bmethod = 
				namespace.getMethod( method.getName(), sig );

			if ( bmethod != null )
				return Primitive.unwrap( 
					bmethod.invokeDeclaredMethod( 
					args, declaringInterpreter, callstack, null ) );

			
			bmethod = namespace.getMethod( "invoke", 
				new Class [] { null, null } );

			
			if ( bmethod != null )
				return Primitive.unwrap( 
					bmethod.invokeDeclaredMethod( 
					new Object [] { method.getName(), args }, 
					declaringInterpreter, callstack, null ) );

			
			
			if ( method.getName().equals("toString" ) )
				return toStringShowInts( proxy.getClass().getInterfaces());

			
			if ( method.getName().equals("hashCode" ) )
				return new Integer(this.hashCode());

			
			if ( method.getName().equals("equals" ) ) {
				Object obj = args[0];
				return new Boolean( proxy == obj );
			}

			throw new EvalError("Bsh script method: "+ method.getName()
				+ " not found in namespace: "+ namespace.name );
		}
	};

}



