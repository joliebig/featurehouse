


package bsh;

import java.lang.reflect.Array;
import java.util.Hashtable;
import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;



class Name implements java.io.Serializable
{
	
	public NameSpace namespace;
	String value = null;
	
	
	
	

	
	
	private String evalName;
	
	private String lastEvalName;
	private static String FINISHED = null; 
	private Object evalBaseObject;	

	private int callstackDepth;		

	
	
	

	
	

	
	

	
	Class asClass;

	
	Class classOfStaticMethod;

	

	private void reset() {
		evalName = value;
		evalBaseObject = null;
		callstackDepth = 0;
	}

	
	
	Name( NameSpace namespace, String s )
	{
		this.namespace = namespace;
		value = s;
	}

	
	public Object toObject( CallStack callstack, Interpreter interpreter ) 
		throws UtilEvalError
	{
		return toObject( callstack, interpreter, false );
	}

	
	synchronized public Object toObject( 
		CallStack callstack, Interpreter interpreter, boolean forceClass ) 
		throws UtilEvalError
	{
		reset();

		Object obj = null;
		while( evalName != null )
			obj = consumeNextObjectField( 
				callstack, interpreter, forceClass, false  );

		if ( obj == null )
			throw new InterpreterError("null value in toObject()");

		return obj;
	}

	private Object completeRound( 
		String lastEvalName, String nextEvalName, Object returnObject )
	{
		if ( returnObject == null )
			throw new InterpreterError("lastEvalName = "+lastEvalName);
		this.lastEvalName = lastEvalName;
		this.evalName = nextEvalName;
		this.evalBaseObject = returnObject;
		return returnObject;
	}

	
	private Object consumeNextObjectField( 	
		CallStack callstack, Interpreter interpreter, 
		boolean forceClass, boolean autoAllocateThis ) 
		throws UtilEvalError
	{
		
		if ( (evalBaseObject == null && !isCompound(evalName) )
			&& !forceClass ) 
		{
			Object obj = resolveThisFieldReference( 
				callstack, namespace, interpreter, evalName, false );

			if ( obj != Primitive.VOID )
				return completeRound( evalName, FINISHED, obj );
		}

		
		String varName = prefix(evalName, 1);
		if ( ( evalBaseObject == null || evalBaseObject instanceof This  )
			&& !forceClass ) 
		{
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

				return completeRound( varName, suffix(evalName), obj );
			}
		}

		
		if ( evalBaseObject == null ) 
		{
			if ( Interpreter.DEBUG ) 
				Interpreter.debug( "trying class: " + evalName);
			
			
			Class clas = null;
			int i = 1;
			String className = null;
			for(; i <= countParts(evalName); i++)
			{
				className = prefix(evalName, i);
				if ( (clas = namespace.getClass(className)) != null )
					break;
			}
		
			if ( clas != null )  {
				return completeRound(
					className,
					suffix( evalName, countParts(evalName)-i ),
					new ClassIdentifier(clas) 
				);
			}
			
			if ( Interpreter.DEBUG ) 
				Interpreter.debug( "not a class, trying var prefix "+evalName );
		}

		
		
		if ( ( evalBaseObject == null || evalBaseObject instanceof This  )
			&& !forceClass && autoAllocateThis )
		{
			NameSpace targetNameSpace = 
				( evalBaseObject == null ) ?  
					namespace : ((This)evalBaseObject).namespace;
			Object obj = new NameSpace( 
				targetNameSpace, "auto: "+varName ).getThis( interpreter );
			targetNameSpace.setVariable( varName, obj, false );
			return completeRound( varName, suffix(evalName), obj );
		}

		
		if ( evalBaseObject == null ) {
			if ( !isCompound(evalName) ) {
				return completeRound( evalName, FINISHED, Primitive.VOID );
			} else
				throw new UtilEvalError(
					"Class or variable not found: " + evalName);
		}

		

		

		if ( evalBaseObject == Primitive.NULL) 
			throw new UtilTargetError( new NullPointerException( 
				"Null Pointer while evaluating: " +value ) );

		if ( evalBaseObject == Primitive.VOID) 
			throw new UtilEvalError(
				"Undefined variable or class name while evaluating: "+value);

		if ( evalBaseObject instanceof Primitive)
			throw new UtilEvalError("Can't treat primitive like an object. "+
			"Error while evaluating: "+value);

		
		if ( evalBaseObject instanceof ClassIdentifier ) 
		{
			Class clas = ((ClassIdentifier)evalBaseObject).getTargetClass();
			String field = prefix(evalName, 1);

			
			
			if ( field.equals("this") )
			{
				
				NameSpace ns = namespace;
				while ( ns != null )
				{
					
					if ( ns.classInstance != null 
						&& ns.classInstance.getClass() == clas 
					)
						return completeRound( 
							field, suffix(evalName), ns.classInstance );
					ns=ns.getParent();
				}
				throw new UtilEvalError(
					"Can't find enclosing 'this' instance of class: "+clas);
			}

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
				throw new UtilEvalError(
					"No static field or inner class: " 
					+ field + " of " + clas );

			return completeRound( field, suffix(evalName), obj );
		}

		
		if ( forceClass )
			throw new UtilEvalError( 
				value +" does not resolve to a class name." );

		

		String field = prefix(evalName, 1);

		
		if ( field.equals("length") && evalBaseObject.getClass().isArray() )
		{
			Object obj = new Primitive(Array.getLength(evalBaseObject));
			return completeRound( field, suffix(evalName), obj );
		}

		
		
		try {
			Object obj = Reflect.getObjectField(evalBaseObject, field);
			return completeRound( field, suffix(evalName), obj );
		} catch(ReflectError e) {  }
	
		
		throw new UtilEvalError(
			"Cannot access field: " + field + ", on object: " + evalBaseObject);
	}

	
	Object resolveThisFieldReference( 
		CallStack callstack, NameSpace thisNameSpace, Interpreter interpreter, 
		String varName, boolean specialFieldsVisible ) 
		throws UtilEvalError
	{
		if ( varName.equals("this") ) 
		{
			
			if ( specialFieldsVisible )
				throw new UtilEvalError("Redundant to call .this on This type");

			
			
	
			This ths = thisNameSpace.getThis( interpreter );
			thisNameSpace= ths.getNameSpace();
			Object result = ths;

			NameSpace classNameSpace = getClassNameSpace( thisNameSpace );
			if ( classNameSpace != null )
			{
				if ( isCompound( evalName ) )
					result = classNameSpace.getThis( interpreter );
				else
					result = classNameSpace.getClassInstance();
			}

			return result;
		}

		
		if ( varName.equals("super") ) 
		{
			
			

			
			This ths = thisNameSpace.getSuper( interpreter );
			thisNameSpace = ths.getNameSpace();
			

	
	
			
			
			if ( 
				thisNameSpace.getParent() != null 
				&& thisNameSpace.getParent().isClass
			)
				ths = thisNameSpace.getParent().getThis( interpreter );

			return ths;
		}

		Object obj = null;

		if ( varName.equals("global") )
			obj = thisNameSpace.getGlobal( interpreter );

		if ( obj == null && specialFieldsVisible ) 
		{
			if (varName.equals("namespace"))
				obj = thisNameSpace;
			else if (varName.equals("variables"))
				obj = thisNameSpace.getVariableNames();
			else if (varName.equals("methods"))
				obj = thisNameSpace.getMethodNames();
			else if ( varName.equals("interpreter") )
				if ( lastEvalName.equals("this") )
					obj = interpreter;
				else
					throw new UtilEvalError(
						"Can only call .interpreter on literal 'this'");
		}

		if ( obj == null && specialFieldsVisible && varName.equals("caller") )
		{
			if ( lastEvalName.equals("this") || lastEvalName.equals("caller") ) 
			{
				
				if ( callstack == null )
					throw new InterpreterError("no callstack");
				obj = callstack.get( ++callstackDepth ).getThis( 
					interpreter ); 
			}
			else
				throw new UtilEvalError(
				"Can only call .caller on literal 'this' or literal '.caller'");

			
			return obj;
		}

		if ( obj == null && specialFieldsVisible 
			&& varName.equals("callstack") )
		{
			if ( lastEvalName.equals("this") ) 
			{
				
				if ( callstack == null )
					throw new InterpreterError("no callstack");
				obj = callstack;
			}
			else
				throw new UtilEvalError(
				"Can only call .callstack on literal 'this'");
		}


		if ( obj == null )
			obj = thisNameSpace.getVariable(varName);

		if ( obj == null )
			throw new InterpreterError("null this field ref:"+varName);

		return obj;
	}

	
	static NameSpace getClassNameSpace( NameSpace thisNameSpace ) 
	{
		NameSpace classNameSpace = null;
		
		
		if ( thisNameSpace.isClass )
			return thisNameSpace;

		if ( thisNameSpace.isMethod 
			&& thisNameSpace.getParent() != null 
			
			&& thisNameSpace.getParent().isClass
		)
			return thisNameSpace.getParent();

		return null;
	}

	
	synchronized public Class toClass() 
		throws ClassNotFoundException, UtilEvalError
	{
		if ( asClass != null )
			return asClass;

		reset();

		
		if ( evalName.equals("var") )
			return asClass = null;

		
		Class clas = namespace.getClass( evalName );

		if ( clas == null ) 
		{
			
			Object obj = null;
			try {
				
				
				obj = toObject( null, null, true );  
			} catch ( UtilEvalError  e ) { }; 
		
			if ( obj instanceof ClassIdentifier )
				clas = ((ClassIdentifier)obj).getTargetClass();
		}

		if ( clas == null )
			throw new ClassNotFoundException(
				"Class: " + value+ " not found in namespace");

		asClass = clas;
		return asClass;
	}

	
	synchronized public LHS toLHS( 
		CallStack callstack, Interpreter interpreter )
		throws UtilEvalError
	{
		
		reset();
		LHS lhs;

		
		if ( !isCompound(evalName) ) 
		{
			if ( evalName.equals("this") )
				throw new UtilEvalError("Can't assign to 'this'." );

			
			lhs = new LHS( namespace, evalName, false);
			return lhs;
		}

		
		Object obj = null;
		try {
			while( evalName != null && isCompound( evalName ) )
			{
				obj = consumeNextObjectField( callstack, interpreter, 
					false, true );
			}
		} 
		catch( UtilEvalError e ) {
			throw new UtilEvalError( "LHS evaluation: " + e.getMessage() );
		}

		
		if ( evalName == null && obj instanceof ClassIdentifier )
			throw new UtilEvalError("Can't assign to class: " + value );

		if ( obj == null )
			throw new UtilEvalError("Error in LHS: " + value );

		
		if ( obj instanceof This )
		{
			
			if ( 
				evalName.equals("namespace")
				|| evalName.equals("variables")
				|| evalName.equals("methods")
				|| evalName.equals("caller")
			)
				throw new UtilEvalError(
					"Can't assign to special variable: "+evalName );

			Interpreter.debug("found This reference evaluating LHS");
			
			boolean localVar = !lastEvalName.equals("super");
			return new LHS( ((This)obj).namespace, evalName, localVar );
		}

		if ( evalName != null )
		{
			try {
				if ( obj instanceof ClassIdentifier ) 
				{
					Class clas = ((ClassIdentifier)obj).getTargetClass();
					lhs = Reflect.getLHSStaticField(clas, evalName);
					return lhs;
				} else {
					lhs = Reflect.getLHSObjectField(obj, evalName);
					return lhs;
				}
			} catch(ReflectError e) {
				throw new UtilEvalError("Field access: "+e);
			}
		}

		throw new InterpreterError("Internal error in lhs...");
	}
	
    
    public Object invokeMethod(
		Interpreter interpreter, Object[] args, CallStack callstack,
		SimpleNode callerInfo
	)
        throws UtilEvalError, EvalError, ReflectError, InvocationTargetException
    {
        String methodName = Name.suffix(value, 1);
		BshClassManager bcm = interpreter.getClassManager();
		NameSpace namespace = callstack.top();

		
		
		
        if ( classOfStaticMethod != null )
		{
			return Reflect.invokeStaticMethod( 
				bcm, classOfStaticMethod, methodName, args );
		}

		if ( !Name.isCompound(value) )
			return invokeLocalMethod( 
				interpreter, args, callstack, callerInfo );

		
		
		
		
		

        String prefix = Name.prefix(value);

		
		if ( prefix.equals("super") && Name.countParts(value) == 2 )
		{
			NameSpace classNameSpace = getClassNameSpace( namespace );
			if ( classNameSpace != null )
			{
				Object instance = classNameSpace.getClassInstance();
				return ClassGenerator.getClassGenerator()
					.invokeSuperclassMethod( bcm, instance, methodName, args );
			}
		}

        
        Name targetName = namespace.getNameResolver( prefix );
        Object obj = targetName.toObject( callstack, interpreter );

		if ( obj == Primitive.VOID ) 
			throw new UtilEvalError( "Attempt to resolve method: "+methodName
					+"() on undefined variable or class name: "+targetName);

        
        if ( !(obj instanceof ClassIdentifier) ) {

            if (obj instanceof Primitive) {

                if (obj == Primitive.NULL)
                    throw new UtilTargetError( new NullPointerException( 
						"Null Pointer in Method Invocation" ) );

                
                
                
                
				if ( Interpreter.DEBUG )
                	interpreter.debug(
					"Attempt to access method on primitive..." 
					+ " allowing bsh.Primitive to peek through for debugging");
            }

            
            return Reflect.invokeObjectMethod(
				obj, methodName, args, interpreter, callstack, callerInfo );
        }

		

        
        if ( Interpreter.DEBUG ) 
        	Interpreter.debug("invokeMethod: trying static - " + targetName);

        Class clas = ((ClassIdentifier)obj).getTargetClass();

		
		classOfStaticMethod = clas;
		
        if ( clas != null )
			return Reflect.invokeStaticMethod( bcm, clas, methodName, args );

        
		throw new UtilEvalError("invokeMethod: unknown target: " + targetName);
    }

	
	
    private Object invokeLocalMethod( 
		Interpreter interpreter, Object[] args, CallStack callstack,
		SimpleNode callerInfo
	)
        throws EvalError
    {
        if ( Interpreter.DEBUG ) 
        	Interpreter.debug( "invokeLocalMethod: " + value );
		if ( interpreter == null )
			throw new InterpreterError(
				"invokeLocalMethod: interpreter = null");

		String commandName = value;
		Class [] argTypes = Types.getTypes( args );

        
        BshMethod meth = null;
		try {
			meth = namespace.getMethod( commandName, argTypes );
		} catch ( UtilEvalError e ) {
			throw e.toEvalError(
				"Local method invocation", callerInfo, callstack );
		}

		
        if ( meth != null )
			return meth.invoke( args, interpreter, callstack, callerInfo );

		BshClassManager bcm = interpreter.getClassManager();

		

		Object commandObject;
		try {
			commandObject = namespace.getCommand( 
				commandName, argTypes, interpreter );
		} catch ( UtilEvalError e ) {
			throw e.toEvalError("Error loading command: ", 
				callerInfo, callstack );
		}

		
		if ( commandObject == null )
		{
			
			
			
			BshMethod invokeMethod = null;
			try {
				invokeMethod = namespace.getMethod( 
					"invoke", new Class [] { null, null } );
			} catch ( UtilEvalError e ) {
				throw e.toEvalError(
					"Local method invocation", callerInfo, callstack );
			}

			if ( invokeMethod != null )
				return invokeMethod.invoke( 
					new Object [] { commandName, args }, 
					interpreter, callstack, callerInfo );

            throw new EvalError( "Command not found: " 
				+StringUtil.methodString( commandName, argTypes ), 
				callerInfo, callstack );
		}

		if ( commandObject instanceof BshMethod )
			return ((BshMethod)commandObject).invoke( 
				args, interpreter, callstack, callerInfo );

		if ( commandObject instanceof Class )
			try {
				return Reflect.invokeCompiledCommand( 
					((Class)commandObject), args, interpreter, callstack );
			} catch ( UtilEvalError e ) {
				throw e.toEvalError("Error invoking compiled command: ",
				callerInfo, callstack );
			}

		throw new InterpreterError("invalid command type");
    }



	
	

	public static boolean isCompound(String value)
	{
		return value.indexOf('.') != -1 ;
		
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
		if (parts < 1 )
			return null;

		int count = 0;
		int index = -1;

		while( ((index = value.indexOf('.', index + 1)) != -1) 
			&& (++count < parts) )
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
		if (parts < 1)
			return null;

		int count = 0;
		int index = value.length() + 1;

		while ( ((index = value.lastIndexOf('.', index - 1)) != -1) 
			&& (++count < parts) );

		return (index == -1) ? value : value.substring(index + 1);
	}

	


	public String toString() { return value; }

}

