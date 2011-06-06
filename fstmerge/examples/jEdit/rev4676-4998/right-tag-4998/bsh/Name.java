


package bsh;

import java.lang.reflect.Array;
import java.util.Hashtable;
import java.io.*;
import java.lang.reflect.InvocationTargetException;



class Name implements java.io.Serializable
{
	
	public NameSpace namespace;
	String value = null;
	
	
	
	

	
	private String evalName;		
	private Object evalBaseObject;	

	private int callstackDepth;		
	
	private boolean literalThisReference;
	
	private boolean literalCallerReference;

	
	
	


	private void reset() {
		evalName = value;
		evalBaseObject = null;
		callstackDepth = 0;
		literalThisReference=false;
		literalCallerReference=false;
	}

	
	public Name(NameSpace namespace, String s)
	{
		this.namespace = namespace;
		value = s;
	}

	
	public Object toObject( CallStack callstack, Interpreter interpreter ) 
		throws EvalError
	{
		return toObject( callstack, interpreter, false );
	}

	
	synchronized public Object toObject( 
		CallStack callstack, Interpreter interpreter, boolean forceClass ) 
		throws EvalError
	{
		reset();

		Object obj = null;
		while( evalName != null )
			obj = consumeNextObjectField( callstack, interpreter, forceClass );

		if ( obj == null )
			throw new InterpreterError("null value in toObject()");

		return obj;
	}

	
	private Object consumeNextObjectField( 	
		CallStack callstack, Interpreter interpreter, boolean forceClass ) 
		throws EvalError
	{
		
		if ( (evalBaseObject == null && !isCompound(evalName) )
			&& !forceClass ) 
		{
			Object obj = resolveThisFieldReference( 
				callstack, namespace, interpreter, evalName, false );

			if ( obj != Primitive.VOID ) {
				evalName = null; 
				return evalBaseObject = obj;  
			}
		}

		
		if ( ( evalBaseObject == null || evalBaseObject instanceof This  )
			&& !forceClass ) 
		{
			String varName = prefix(evalName, 1);
			if ( Interpreter.DEBUG ) 
				Interpreter.debug("trying to resolve variable: " + varName);
			Object obj;
			if ( evalBaseObject == null ) {
				obj = resolveThisFieldReference( 
					callstack, namespace, interpreter, varName, false );
			} else {
				
				obj = resolveThisFieldReference( 
					callstack, ((This)evalBaseObject).namespace, 
					interpreter, varName, true );
			}

			if ( obj != Primitive.VOID ) 
			{
				
				if ( Interpreter.DEBUG ) 
					Interpreter.debug( "resolved variable: " + varName + 
					" in namespace: "+namespace);
				evalName = suffix(evalName);
				return evalBaseObject = obj;
			}
		}

		
		if ( evalBaseObject == null ) {
			if ( Interpreter.DEBUG ) 
				Interpreter.debug( "trying class: " + evalName);
			
			
			Class clas = null;
			int i = 1;
			for(; i <= countParts(evalName); i++)
				if ( (clas = namespace.getClass(prefix(evalName, i))) != null )
					break;
		
			if( clas != null )  {
				evalName = suffix(evalName, countParts(evalName) - i);
				return ( evalBaseObject = new ClassIdentifier(clas) );
			}
			
			if ( Interpreter.DEBUG ) 
				Interpreter.debug( "not a class, trying var prefix "+evalName );
		}


		
		if ( evalBaseObject == null ) {
			if( !isCompound(evalName) ) {
				evalName = null; 
				return evalBaseObject = Primitive.VOID;  
			} else
				throw new EvalError(
					"Class or variable not found:" + evalName);
		}

		

		

		if(evalBaseObject == Primitive.NULL) 
			throw new TargetError( "Null Pointer while evaluating: "
				+value, new NullPointerException() );

		if(evalBaseObject == Primitive.VOID) 
			throw new EvalError(
				"Undefined variable or class name while evaluating: "+value);

		if(evalBaseObject instanceof Primitive)
			throw new EvalError("Can't treat primitive like an object. "+
			"Error while evaluating: "+value);

		
		if ( evalBaseObject instanceof ClassIdentifier ) 
		{
			Class clas = ((ClassIdentifier)evalBaseObject).getTargetClass();
			String field = prefix(evalName, 1);

			Object obj = null;
			
			try {
				if ( Interpreter.DEBUG ) 
					Interpreter.debug("Name call to getStaticField, class: " 
						+clas+", field:"+field);
				obj = Reflect.getStaticField(clas, field);
			} catch( ReflectError e ) { 
				if ( Interpreter.DEBUG ) 
					Interpreter.debug("field reflect error: "+e);
			}

			
			if ( obj == null ) {
				String iclass = clas.getName()+"$"+field;
				Class c = namespace.getClass( iclass );
				if ( c != null )
					obj = new ClassIdentifier(c);
			}

			if ( obj == null )
				throw new EvalError(
					"No static field or inner class: " + field + " of " + clas);

			evalName = suffix(evalName);
			return (evalBaseObject = obj);
		}

		
		if ( forceClass )
			throw new EvalError( value +" does not resolve to a class name." );

		

		String field = prefix(evalName, 1);

		
		if(field.equals("length") && evalBaseObject.getClass().isArray())
		{
			Object obj = new Primitive(Array.getLength(evalBaseObject));
			evalName = suffix(evalName);
			return (evalBaseObject = obj);
		}

		
		
		try
		{
			Object obj = Reflect.getObjectField(evalBaseObject, field);
			evalName = suffix(evalName);
			return (evalBaseObject = obj);
		}
		catch(ReflectError e) {  }
	
		
		throw new EvalError(
			"Cannot access field: " + field + ", on object: " + evalBaseObject);
	}

	
	Object resolveThisFieldReference( 
		CallStack callstack, NameSpace thisNamespace, Interpreter interpreter, 
		String varName, boolean specialFieldsVisible ) 
		throws EvalError
	{
		Object obj = null;
		
		boolean 
			wasThis = false,		
			wasCaller = false;

		if ( varName.equals("this") ) {
			
			
			if ( specialFieldsVisible )
				throw new EvalError("Redundant to call .this on This type");
			obj = thisNamespace.getThis( interpreter );
			wasThis = true;
		} 

		if ( obj == null ) {
			if ( varName.equals("super") )
				obj = thisNamespace.getSuper().getThis( interpreter );
			else if ( varName.equals("global") )
				obj = thisNamespace.getGlobal().getThis( interpreter );
		}

		if ( obj == null && specialFieldsVisible ) {
			if (varName.equals("namespace"))
				obj = thisNamespace;
			else if (varName.equals("variables"))
				obj = thisNamespace.getVariableNames();
			else if (varName.equals("methods"))
				obj = thisNamespace.getMethodNames();
			else if ( varName.equals("interpreter") )
				if ( literalThisReference )
					obj = interpreter;
				else
					throw new EvalError(
						"Can only call .interpreter on literal 'this'");
		}

		if ( obj == null && specialFieldsVisible && varName.equals("caller") )
		{
			if ( literalThisReference || literalCallerReference ) 
			{
				
				if ( callstack == null )
					throw new InterpreterError("no callstack");
				obj = callstack.get( ++callstackDepth ).getThis( 
					interpreter ); 
			}
			else
				throw new EvalError(
				"Can only call .caller on literal 'this' or literal '.caller'");

			wasCaller = true;
		}

		if ( obj == null && specialFieldsVisible 
			&& varName.equals("callstack") )
		{
			if ( literalThisReference ) 
			{
				
				if ( callstack == null )
					throw new InterpreterError("no callstack");
				obj = callstack;
			}
			else
				throw new EvalError(
				"Can only call .callstack on literal 'this'");
		}


		if ( obj == null )
			obj = thisNamespace.getVariable(varName);

		literalThisReference = wasThis;
		literalCallerReference = wasCaller;
		return obj;
	}

	
	synchronized public Class toClass() throws EvalError 
	{
		reset();

		
		Class clas = namespace.getClass(evalName);

		if ( clas == null ) {
			
			Object obj = null;
			try {
				
				
				obj = toObject( null, null, true );  
			} catch ( EvalError  e ) { }; 
		
			if ( obj instanceof ClassIdentifier )
				clas = ((ClassIdentifier)obj).getTargetClass();
		}

		if( clas == null )
			throw new EvalError(
				"Class: " + value+ " not found in namespace");

		return clas;
	}

	
	synchronized public LHS toLHS( 
		CallStack callstack, Interpreter interpreter )
		throws EvalError
	{
		reset();

		

		
		if(!isCompound(evalName)) {
			
				
			return new LHS(namespace,evalName);
		}

		
		Object obj = null;
		try
		{
			while(isCompound(evalName))
				obj = consumeNextObjectField( callstack, interpreter, false );
		}
		catch( EvalError e )
		{
			throw new EvalError("LHS evaluation: " + e);
		}

		if ( obj == null )
			throw new InterpreterError("internal error 2893749283");

		if(obj instanceof This)
		{
			Interpreter.debug("found This reference evaluating LHS");
			return new LHS(((This)obj).namespace, evalName);
		}

		if(evalName != null)
		{
			try
			{



				if ( obj instanceof ClassIdentifier ) 
				{
					Class clas = ((ClassIdentifier)obj).getTargetClass();
					return Reflect.getLHSStaticField(clas, evalName);
				} else
					return Reflect.getLHSObjectField(obj, evalName);
			} catch(ReflectError e)
			{
				throw new EvalError("Field access: "+e);
			}
		}

		throw new InterpreterError("Internal error in lhs...");

	

	}
	
	private BshMethod toLocalMethod( Object [] args )
	{
		Class [] sig = Reflect.getTypes( args );
		return namespace.getMethod( value, sig );
	}


    
    public Object invokeMethod(
		Interpreter interpreter, Object[] args, CallStack callstack,
		SimpleNode callerInfo
	)
        throws EvalError, ReflectError, InvocationTargetException
    {
        if ( !Name.isCompound(value) )
            return invokeLocalMethod(interpreter, args, callstack, callerInfo);

        
        Name targetName = namespace.getNameResolver( Name.prefix(value));
        String methodName = Name.suffix(value, 1);

        Object obj = targetName.toObject( callstack, interpreter );

		if ( obj == Primitive.VOID ) 
			throw new EvalError( "Attempt to invoke method: "+methodName
					+"() on undefined variable or class name: "+targetName);

        
        if ( !(obj instanceof Name.ClassIdentifier) ) {

            if (obj instanceof Primitive) {

                if (obj == Primitive.NULL)
                    throw new TargetError( "Null Pointer in Method Invocation",
					new NullPointerException() );

                
                
                
                
                interpreter.error("Attempt to access method on primitive..." +
                    " allowing bsh.Primitive to peek through for debugging");
            }

            
            return Reflect.invokeObjectMethod(
				interpreter, obj, methodName, args, callerInfo);
        }

        
        if ( Interpreter.DEBUG ) 
			Interpreter.debug("invokeMethod: trying static - " + targetName);

        Class clas = ((Name.ClassIdentifier)obj).getTargetClass();
        if (clas != null)
            return Reflect.invokeStaticMethod(clas, methodName, args);

        
		throw new EvalError("unknown target: " + targetName);
    }

	
    public Object invokeLocalMethod( 
		Interpreter interpreter, Object[] args, CallStack callstack,
		SimpleNode callerInfo
	)
        throws EvalError, ReflectError, InvocationTargetException
    {
        if ( Interpreter.DEBUG ) 
			Interpreter.debug("invoke local method: " + value);

        
        BshMethod meth = toLocalMethod( args );
        if ( meth != null )
            return meth.invokeDeclaredMethod( args, interpreter, callstack, callerInfo );
        else
            if ( Interpreter.DEBUG ) 
				Interpreter.debug("no locally declared method: " + value);

        
		
        String commandName = "commands/" + value + ".bsh";
        InputStream in = Interpreter.class.getResourceAsStream(commandName);
        if (in != null)
        {
            if ( Interpreter.DEBUG ) 
				Interpreter.debug("loading resource: " + commandName);

			if ( interpreter == null )
				throw new InterpreterError("2234432 interpreter = null");

            interpreter.eval( 
				new InputStreamReader(in), namespace, commandName);

            
            meth = toLocalMethod( args );
            if(meth != null)
                return meth.invokeDeclaredMethod( 
					args, interpreter, callstack, callerInfo );
            else
                throw new EvalError("Loaded resource: " + commandName +
                    "had an error or did not contain the correct method");
        }

        
        commandName = "bsh.commands." + value;
        
        Class c = BshClassManager.classForName( commandName );
        if(c == null)
            throw new EvalError("Command not found: " + value);

        
        Object[] invokeArgs = new Object[args.length + 2];
        invokeArgs[0] = interpreter;
        invokeArgs[1] = namespace;
        System.arraycopy(args, 0, invokeArgs, 2, args.length);
        try
        {
            return Reflect.invokeStaticMethod(c, "invoke", invokeArgs);
        }
        catch(ReflectError e)
        {
            if ( Interpreter.DEBUG ) 
				Interpreter.debug("invoke command args error:" + e);
            
        }
        
        try
        {
            String s = (String)Reflect.invokeStaticMethod(c, "usage", null);
            interpreter.println(s);
            return Primitive.VOID;
        }
        catch(ReflectError e)
        {
            if ( Interpreter.DEBUG ) Interpreter.debug("usage threw: " + e);
            throw new EvalError("Wrong number or type of args for command");
        }
    }

	

	static boolean isCompound(String value)
	{
		return countParts(value) > 1;
	}

	static int countParts(String value)
	{
		if(value == null)
			return 0;

		int count = 0;
		int index = -1;
		while((index = value.indexOf('.', index + 1)) != -1)
			count++;
		return count + 1;
	}

	static String prefix(String value)
	{
		if(!isCompound(value))
			return null;

		return prefix(value, countParts(value) - 1);
	}

	static String prefix(String value, int parts)
	{
		if(parts < 1)
			return null;

		int count = 0;
		int index = -1;

		while(((index = value.indexOf('.', index + 1)) != -1) && (++count < parts))
		{ ; }

		return (index == -1) ? value : value.substring(0, index);
	}

	static String suffix(String name)
	{
		if(!isCompound(name))
			return null;

		return suffix(name, countParts(name) - 1);
	}

	public static String suffix(String value, int parts)
	{
		if(parts < 1)
			return null;

		int count = 0;
		int index = value.length() + 1;

		while(((index = value.lastIndexOf('.', index - 1)) != -1) && (++count < parts))
		{ ; }

		return (index == -1) ? value : value.substring(index + 1);
	}

	


	public String toString() { return value; }

	static class ClassIdentifier {
		Class clas;

		public ClassIdentifier( Class clas ) {
			this.clas = clas;
		}

		public Class getTargetClass() {
			return clas;
		}

		public String toString() {
			return "Class Identifier: "+clas.getName();
		}
	}


}

