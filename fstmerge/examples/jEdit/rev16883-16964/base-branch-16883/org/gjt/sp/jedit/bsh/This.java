


package org.gjt.sp.jedit.bsh;


public class This implements java.io.Serializable, Runnable
{
    
    NameSpace namespace;

    
    transient Interpreter declaringInterpreter;

    
    static This getThis(
        NameSpace namespace, Interpreter declaringInterpreter )
    {
        try {
            Class c;
            if ( Capabilities.canGenerateInterfaces() )
                c = Class.forName( "org.gjt.sp.jedit.bsh.XThis" );
            else if ( Capabilities.haveSwing() )
                c = Class.forName( "org.gjt.sp.jedit.bsh.JThis" );
            else
                return new This( namespace, declaringInterpreter );

            return (This)Reflect.constructObject( c,
                new Object [] { namespace, declaringInterpreter } );

        } catch ( Exception e ) {
            throw new InterpreterError("internal error 1 in This: "+e);
        }
    }

    
    
    public Object getInterface( Class clas )
        throws UtilEvalError
    {
        if ( clas.isInstance( this ) )
            return this;
        else
            throw new UtilEvalError( "Dynamic proxy mechanism not available. "
            + "Cannot construct interface type: "+clas );
    }

    
    public Object getInterface( Class [] ca )
        throws UtilEvalError
    {
        for(int i=0; i<ca.length; i++)
            if ( !(ca[i].isInstance( this )) )
                throw new UtilEvalError(
                    "Dynamic proxy mechanism not available. "
                    + "Cannot construct interface type: "+ca[i] );

        return this;
    }

    
    protected This( NameSpace namespace, Interpreter declaringInterpreter ) {
        this.namespace = namespace;
        this.declaringInterpreter = declaringInterpreter;
        
    }

    public NameSpace getNameSpace() {
        return namespace;
    }

    public String toString() {
        return "'this' reference to Bsh object: " + namespace;
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
        
        return invokeMethod(
            name, args, null, null, null,
            false );
    }

    
    
    public Object invokeMethod(
        String methodName, Object [] args,
        Interpreter interpreter, CallStack callstack, SimpleNode callerInfo,
        boolean declaredOnly  )
        throws EvalError
    {
        
        if ( args != null )
        {
            Object [] oa = new Object [args.length];
            for(int i=0; i<args.length; i++)
                oa[i] = ( args[i] == null ? Primitive.NULL : args[i] );
            args = oa;
        }

        if ( interpreter == null )
            interpreter = declaringInterpreter;
        if ( callstack == null )
            callstack = new CallStack( namespace );
        if ( callerInfo == null )
            callerInfo = SimpleNode.JAVACODE;

        
        Class [] types = Types.getTypes( args );
        BshMethod bshMethod = null;
        try {
            bshMethod = namespace.getMethod( methodName, types, declaredOnly );
        } catch ( UtilEvalError e ) {
            
        }

        if ( bshMethod != null )
            return bshMethod.invoke( args, interpreter, callstack, callerInfo );

        
        
        if ( methodName.equals("toString" ) )
            return toString();

        
        if ( methodName.equals("hashCode" ) )
            return new Integer(this.hashCode());

        
        if ( methodName.equals("equals" ) ) {
            Object obj = args[0];
            return new Boolean( this == obj );
        }

        
        
        
        try {
            bshMethod = namespace.getMethod(
                "invoke", new Class [] { null, null } );
        } catch ( UtilEvalError e ) {  }

        
        if ( bshMethod != null )
            return bshMethod.invoke( new Object [] { methodName, args },
                interpreter, callstack, callerInfo );

        throw new EvalError("Method " +
            StringUtil.methodString( methodName, types ) +
            " not found in bsh scripted object: "+ namespace.getName(),
            callerInfo, callstack );
    }

    
    public static void bind(
        This ths, NameSpace namespace, Interpreter declaringInterpreter )
    {
        ths.namespace.setParent( namespace );
        ths.declaringInterpreter = declaringInterpreter;
    }

    
    static boolean isExposedThisMethod( String name )
    {
        return
            name.equals("getClass")
            || name.equals("invokeMethod")
            || name.equals("getInterface")
            
            || name.equals("wait")
            || name.equals("notify")
            || name.equals("notifyAll");
    }

}

