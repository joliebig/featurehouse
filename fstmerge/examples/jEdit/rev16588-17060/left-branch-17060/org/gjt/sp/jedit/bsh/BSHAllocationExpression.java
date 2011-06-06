


package org.gjt.sp.jedit.bsh;

import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;


class BSHAllocationExpression extends SimpleNode
{
    BSHAllocationExpression(int id) { super(id); }
	private static int innerClassCount = 0;
	
    public Object eval( CallStack callstack, Interpreter interpreter) 
		throws EvalError
    {
        
        SimpleNode type = (SimpleNode)jjtGetChild(0);

        
        SimpleNode args = (SimpleNode)jjtGetChild(1);

        if ( type instanceof BSHAmbiguousName )
        {
            BSHAmbiguousName name = (BSHAmbiguousName)type;

            if (args instanceof BSHArguments)
                return objectAllocation(name, (BSHArguments)args, 
					callstack, interpreter );
            else
                return objectArrayAllocation(name, (BSHArrayDimensions)args, 
					callstack, interpreter );
        }
        else
            return primitiveArrayAllocation((BSHPrimitiveType)type,
                (BSHArrayDimensions)args, callstack, interpreter );
    }

    private Object objectAllocation(
		BSHAmbiguousName nameNode, BSHArguments argumentsNode, 
		CallStack callstack, Interpreter interpreter 
	) 
		throws EvalError
    {
		NameSpace namespace = callstack.top();

        Object[] args = argumentsNode.getArguments( callstack, interpreter );
        if ( args == null)
            throw new EvalError( "Null args in new.", this, callstack );

		
        Object obj = nameNode.toObject( 
			callstack, interpreter, false );

		

        obj = nameNode.toObject( 
			callstack, interpreter, true );

        Class type = null;
		if ( obj instanceof ClassIdentifier )
        	type = ((ClassIdentifier)obj).getTargetClass();
		else
			throw new EvalError( 
				"Unknown class: "+nameNode.text, this, callstack );

		
		boolean hasBody = jjtGetNumChildren() > 2;

		if ( hasBody ) 
		{
        	BSHBlock body = (BSHBlock)jjtGetChild(2);
			if ( type.isInterface() )
				return constructWithInterfaceBody( 
					type, args, body, callstack, interpreter );
			else
				return constructWithClassBody( 
					type, args, body, callstack, interpreter );
		} else
			return constructObject( type, args, callstack );
    }

	private Object constructObject( 
		Class type, Object[] args, CallStack callstack ) 
		throws EvalError
	{
		Object obj;
        try {
            obj = Reflect.constructObject( type, args );
        } catch ( ReflectError e) {
            throw new EvalError(
				"Constructor error: " + e.getMessage(), this, callstack );
        } catch(InvocationTargetException e) {
			
			Interpreter.debug("The constructor threw an exception:\n\t" +
				e.getTargetException());
            throw new TargetError(
				"Object constructor", e.getTargetException(), 
				this, callstack, true);
        }

		String className = type.getName();
		
		if ( className.indexOf("$") == -1 )
			return obj;

		
		
		
		

		
		This ths = callstack.top().getThis( null );
		NameSpace instanceNameSpace = 
			Name.getClassNameSpace( ths.getNameSpace() );
		
		
		
		
		
		if ( instanceNameSpace != null 
			&& className.startsWith( instanceNameSpace.getName() +"$") 
		)
		{
			try {
				ClassGenerator.getClassGenerator().setInstanceNameSpaceParent(
					obj, className, instanceNameSpace );
			} catch ( UtilEvalError e ) {
				throw e.toEvalError( this, callstack );
			}
		}

		return obj;
	}

	private Object constructWithClassBody( 
		Class type, Object[] args, BSHBlock block,
		CallStack callstack, Interpreter interpreter ) 
		throws EvalError
	{
		String name = callstack.top().getName() + "$" + (++innerClassCount);
		Modifiers modifiers = new Modifiers();
		modifiers.addModifier( Modifiers.CLASS, "public" );
		Class clas;
		try {
			clas = ClassGenerator.getClassGenerator() .generateClass( 
				name, modifiers, null, type, 
				block, false, callstack, interpreter );
		} catch ( UtilEvalError e ) {
			throw e.toEvalError( this, callstack );
		}
		try {
			return Reflect.constructObject( clas, args );
		} catch ( Exception e ) {
			if ( e instanceof InvocationTargetException )
				e = (Exception)((InvocationTargetException)e)
					.getTargetException();
			throw new EvalError(
				"Error constructing inner class instance: "+e, this, callstack
			);
		}
	}

	private Object constructWithInterfaceBody( 
		Class type, Object[] args, BSHBlock body,
		CallStack callstack, Interpreter interpreter ) 
		throws EvalError
	{
		NameSpace namespace = callstack.top();
		NameSpace local = new NameSpace(namespace, "AnonymousBlock");
		callstack.push(local);
		body.eval( callstack, interpreter, true );
		callstack.pop();
		
		
		local.importStatic( type );
		try {
			return local.getThis(interpreter).getInterface( type );
		} catch ( UtilEvalError e ) {
			throw e.toEvalError( this, callstack );
		}
	}

    private Object objectArrayAllocation(
		BSHAmbiguousName nameNode, BSHArrayDimensions dimensionsNode, 
		CallStack callstack, Interpreter interpreter 
	) 
		throws EvalError
    {
		NameSpace namespace = callstack.top();
        Class type = nameNode.toClass( callstack, interpreter );
        if ( type == null )
            throw new EvalError( "Class " + nameNode.getName(namespace) 
				+ " not found.", this, callstack );

		return arrayAllocation( dimensionsNode, type, callstack, interpreter );
    }

    private Object primitiveArrayAllocation(
		BSHPrimitiveType typeNode, BSHArrayDimensions dimensionsNode, 
		CallStack callstack, Interpreter interpreter 
	) 
		throws EvalError
    {
        Class type = typeNode.getType();

		return arrayAllocation( dimensionsNode, type, callstack, interpreter );
    }

	private Object arrayAllocation( 
		BSHArrayDimensions dimensionsNode, Class type, 
		CallStack callstack, Interpreter interpreter )
		throws EvalError
	{
		
        Object result = dimensionsNode.eval( type, callstack, interpreter );
        if ( result != Primitive.VOID )
            return result;
		else
			return arrayNewInstance( type, dimensionsNode, callstack );
	}

	
	private Object arrayNewInstance( 
		Class type, BSHArrayDimensions dimensionsNode, CallStack callstack )
		throws EvalError
	{
		if ( dimensionsNode.numUndefinedDims > 0 )
		{
            Object proto = Array.newInstance( 
				type, new int [dimensionsNode.numUndefinedDims] ); 
			type = proto.getClass();
		}

        try {
            return Array.newInstance( 
				type, dimensionsNode.definedDimensions);
        } catch( NegativeArraySizeException e1 ) {
			throw new TargetError( e1, this, callstack );
        } catch( Exception e ) {
            throw new EvalError("Can't construct primitive array: " +
                e.getMessage(), this, callstack);
        }
	}
}
