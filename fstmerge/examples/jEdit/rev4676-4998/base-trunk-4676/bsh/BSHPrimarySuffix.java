



package bsh;

import java.util.Hashtable;
import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;

class BSHPrimarySuffix extends SimpleNode
{
	public static final int
		CLASS = 0,
		INDEX = 1,
		NAME = 2,
		PROPERTY = 3;

	public int operation;
	Object index;
	public String field;

	BSHPrimarySuffix(int id) { super(id); }

	
	public Object doSuffix(
		Object obj, CallStack callstack, Interpreter interpreter) 
		throws EvalError
	{
		
		
		if ( operation == CLASS )
			if ( obj instanceof BSHType ) {
				NameSpace namespace = callstack.top();
				return ((BSHType)obj).getType( namespace );
			} else
				throw new EvalError(
					"Attemp to invoke .class on non class.", this);

		

		
		if ( obj instanceof SimpleNode )
			if ( obj instanceof BSHAmbiguousName )
				obj = ((BSHAmbiguousName)obj).toObject(callstack, interpreter);
			else
				obj = ((SimpleNode)obj).eval(callstack, interpreter);	

		try
		{
			switch(operation)
			{
				case INDEX:
					return doIndex(obj, callstack, interpreter );

				case NAME:
					return doName(obj, callstack, interpreter );

				case PROPERTY:
					return doProperty(obj, callstack, interpreter );

				default:
					throw new InterpreterError("LHS suffix");
			} 
		}
		catch(ReflectError e)
		{
			throw new EvalError("reflection error: " + e, this);
		}
		catch(InvocationTargetException e)
		{
			throw new TargetError(
				"target exception", e.getTargetException(), this, true);
		}
	}

	
	private Object doName(
		Object obj, CallStack callstack, Interpreter interpreter) 
		throws EvalError, ReflectError, InvocationTargetException
	{
		if(field.equals("length") && obj.getClass().isArray())
			return new Primitive(Array.getLength(obj));
		
		if (jjtGetNumChildren() == 0)
			
			return Reflect.getObjectField(obj, field);
		else
		{
			
			Object[] oa = ((BSHArguments)jjtGetChild(0)).getArguments(
				callstack, interpreter);
			try {
				return Reflect.invokeObjectMethod(interpreter, obj, field, oa, this);
			} catch ( EvalError ee ) {
				
				throw new EvalError( ee.getMessage(), this );
			}
		}
	}

	
	static int getIndexAux(
		Object obj, CallStack callstack, Interpreter interpreter, 
		SimpleNode callerNode ) 
		throws EvalError
	{
		if ( !obj.getClass().isArray() )
			throw new EvalError("Not an array", callerNode );

		int index;
		try {
			Object indexVal = 
				((SimpleNode)callerNode.jjtGetChild(0)).eval( 
					callstack, interpreter );
			if ( !(indexVal instanceof Primitive) )
				indexVal = NameSpace.getAssignableForm( indexVal, Integer.TYPE);
			index = ((Primitive)indexVal).intValue();
		} catch( EvalError e ) {
			Interpreter.debug("doIndex: "+e);
			e.reThrow(
				"You can only index arrays by integer types", callerNode );
			throw new Error("can't get here");
		}

		return index;
	}

	private Object doIndex(
		Object obj, CallStack callstack, Interpreter interpreter) 
		throws EvalError, ReflectError
	{
		int index = getIndexAux( obj, callstack, interpreter, this );
		return Reflect.getIndex(obj, index);
	}

	private Object doProperty( 
		Object obj, CallStack callstack, Interpreter interpreter ) 
		throws EvalError
	{
		if(obj == Primitive.VOID)
			throw new EvalError("Attempt to access property on undefined variable or class name", this);

		if(obj instanceof Primitive)
			throw new EvalError("Attempt to access property on a primitive", this);

		Object value = ((SimpleNode)jjtGetChild(0)).eval(
			callstack, interpreter);
		if(!(value instanceof String))
			throw new EvalError("Property expression must be a String or identifier.", this);

		
		if(obj instanceof Hashtable)
		{
			Object val = ((Hashtable)obj).get((String)value);
			if(val == null)
				val = Primitive.NULL;
			return val;
		}

		try
		{
			return Reflect.getObjectProperty(obj, (String)value);
		}
		catch(ReflectError e)
		{
			Interpreter.debug(e.toString());
			throw new EvalError("No such property: " + value, this);
		}
	}
}

