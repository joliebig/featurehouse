


package bsh;

import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;


class BSHAllocationExpression extends SimpleNode
{
    BSHAllocationExpression(int id) { super(id); }

    public Object eval( CallStack callstack, Interpreter interpreter) 
		throws EvalError
    {
        
        SimpleNode type = (SimpleNode)jjtGetChild(0);

        
        SimpleNode args = (SimpleNode)jjtGetChild(1);

        if ( type instanceof BSHAmbiguousName )
        {
            BSHAmbiguousName name = (BSHAmbiguousName)type;

            if(args instanceof BSHArguments)
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
        Class type = nameNode.toClass(namespace);

		

        Object[] args = argumentsNode.getArguments(callstack, interpreter);
        if(args == null)
            throw new EvalError("Trying to new a class...?", this);

		
		boolean hasBody = jjtGetNumChildren() > 2;

		if ( hasBody ) {
        	BSHBlock body = (BSHBlock)jjtGetChild(2);
			return constructWithBody( 
				type, args, body, callstack, interpreter );
		} else
			return constructObject( type, args );
    }

	private Object constructObject( Class type, Object[] args ) 
		throws EvalError
	{
        try {
            return Reflect.constructObject(type, args);
        } catch(ReflectError e) {
            throw new EvalError("Constructor error: " + e.getMessage(), this);
        } catch(InvocationTargetException e) {
            Interpreter.debug("The constructor threw an exception:\n\t" +
                e.getTargetException());
            throw new TargetError(
				"Object constructor", e.getTargetException(), this, true);
        }
	}

	private Object constructWithBody( 
		Class type, Object[] args, BSHBlock body,
		CallStack callstack, Interpreter interpreter ) 
		throws EvalError
	{
		if ( ! type.isInterface() )
			throw new EvalError(
				"BeanShell cannot extend class types: "+ type );

		NameSpace namespace = callstack.top();


		NameSpace local = new NameSpace(namespace, "anonymous block object");
		callstack.push(local);
		body.eval( callstack, interpreter, true );
		callstack.pop();
		return local.getThis(interpreter).getInterface( type );
	}


    private Object objectArrayAllocation(
		BSHAmbiguousName nameNode, BSHArrayDimensions dimensionsNode, 
		CallStack callstack, Interpreter interpreter 
	) 
		throws EvalError
    {
		NameSpace namespace = callstack.top();
        Class type = nameNode.toClass(namespace);
        if(type == null)
            throw new EvalError(
				"Class " + nameNode.getName(namespace) + " not found.", this);

		
        Object result = dimensionsNode.eval( type, callstack, interpreter );
        if(result != Primitive.VOID)
            return result;
		else
			return arrayNewInstance( type, dimensionsNode );
    }


    private Object primitiveArrayAllocation(
		BSHPrimitiveType typeNode, BSHArrayDimensions dimensionsNode, 
		CallStack callstack, Interpreter interpreter 
	) 
		throws EvalError
    {
        Class type = typeNode.getType();

		
        Object result = dimensionsNode.eval( type, callstack, interpreter );
        if (result != Primitive.VOID) 
            return result;

		return arrayNewInstance( type, dimensionsNode );
    }

	private Object arrayNewInstance( 
		Class type, BSHArrayDimensions dimensionsNode )
		throws EvalError
	{
        try {
            return Array.newInstance(type, dimensionsNode.dimensions);
        } catch( NegativeArraySizeException e1) {
			throw new TargetError("Negative Array Size", e1);
        } catch(Exception e) {
            throw new EvalError("Can't construct primitive array: " +
                e.getMessage(), this);
        }
	}
}
