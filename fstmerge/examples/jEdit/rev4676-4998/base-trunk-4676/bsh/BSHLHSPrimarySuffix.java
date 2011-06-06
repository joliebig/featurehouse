



package bsh;

import java.util.Hashtable;
import java.lang.reflect.InvocationTargetException;

class BSHLHSPrimarySuffix extends SimpleNode
{
	public static final int
		INDEX = 1,
		NAME = 2,
		PROPERTY = 3;

	public int operation;	
	Object index;			
	
	public String field;
	public String method;

	BSHLHSPrimarySuffix(int id) { super(id); }

	public LHS doLHSSuffix(
		Object obj, CallStack callstack, Interpreter interpreter) 
		throws EvalError
	{
		try
		{
			switch(operation)
			{
				case INDEX:
					return doIndex(obj, callstack, interpreter);

				case NAME:
					return doName(obj, callstack, interpreter);

				case PROPERTY:
					return doProperty(obj, callstack, interpreter);

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

	private LHS doName(
		Object obj, CallStack callstack, Interpreter interpreter) 
		throws EvalError, ReflectError, InvocationTargetException 
	{
		if (jjtGetNumChildren() == 0)
			
			return Reflect.getLHSObjectField(obj, field);
		else {
			
			Object[] oa = ((BSHArguments)jjtGetChild(0)).getArguments(
				callstack, interpreter);
			try {
				obj = Reflect.invokeObjectMethod(interpreter, obj, method, oa, this);
			} catch ( EvalError ee ) {
				
				throw new EvalError( ee.getMessage(), this );
			}
			return Reflect.getLHSObjectField(obj, field);
		}
	}

	private LHS doIndex(
		Object obj, CallStack callstack, Interpreter interpreter) 
		throws EvalError, ReflectError
	{
		int index = BSHPrimarySuffix.getIndexAux( 
			obj, callstack, interpreter, this );
		return new LHS(obj, index);
	}

	private LHS doProperty(
		Object obj, CallStack callstack, Interpreter interpreter) 
		throws EvalError, ReflectError
	{
		if(obj == Primitive.VOID)
			throw new EvalError("Attempt to access property on a void type", this);

		else if(obj instanceof Primitive)
			throw new EvalError("Attempt to access property on a primitive", this);

		Object value = ((SimpleNode)jjtGetChild(0)).eval(
			callstack, interpreter);

		if(!(value instanceof String))
			throw new EvalError("Property expression must be a String or identifier.", this);

		if ( Interpreter.DEBUG ) Interpreter.debug("LHS property access: ");
		return new LHS(obj, (String)value);
	}
}

