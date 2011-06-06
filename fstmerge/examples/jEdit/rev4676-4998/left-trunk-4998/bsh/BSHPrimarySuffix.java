


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
		Object obj, boolean toLHS, 
		CallStack callstack, Interpreter interpreter) 
		throws EvalError
	{
		
		
		if ( operation == CLASS )
			if ( obj instanceof BSHType ) {
				if ( toLHS )
					throw new EvalError("Can't assign .class", 
						this, callstack );
				NameSpace namespace = callstack.top();
				return ((BSHType)obj).getType( callstack, interpreter );
			} else
				throw new EvalError(
					"Attempt to use .class suffix on non class.", 
					this, callstack );

		
		if ( obj instanceof SimpleNode )
			if ( obj instanceof BSHAmbiguousName )
				obj = ((BSHAmbiguousName)obj).toObject(callstack, interpreter);
			else
				obj = ((SimpleNode)obj).eval(callstack, interpreter);	
		else
			if ( obj instanceof LHS )
				try {
					obj = ((LHS)obj).getValue();
				} catch ( UtilEvalError e ) {
					throw e.toEvalError( this, callstack );
				}

		try
		{
			switch(operation)
			{
				case INDEX:
					return doIndex( obj, toLHS, callstack, interpreter );

				case NAME:
					return doName( obj, toLHS, callstack, interpreter );

				case PROPERTY:
					return doProperty( toLHS, obj, callstack, interpreter );

				default:
					throw new InterpreterError( "Unknown suffix type" );
			} 
		}
		catch(ReflectError e)
		{
			throw new EvalError("reflection error: " + e, this, callstack );
		}
		catch(InvocationTargetException e)
		{
			throw new TargetError( "target exception", e.getTargetException(), 
				this, callstack, true);
		}
	}

	
	private Object doName(
		Object obj, boolean toLHS, 
		CallStack callstack, Interpreter interpreter) 
		throws EvalError, ReflectError, InvocationTargetException
	{
		try {
			
			if ( field.equals("length") && obj.getClass().isArray() )
				if ( toLHS )
					throw new EvalError(
						"Can't assign array length", this, callstack );
				else
					return new Primitive(Array.getLength(obj));
			
			
			if ( jjtGetNumChildren() == 0 ) 
				if ( toLHS )
					return Reflect.getLHSObjectField(obj, field);
				else
					return Reflect.getObjectField( obj, field );

			
			
			Object[] oa = ((BSHArguments)jjtGetChild(0)).getArguments(
				callstack, interpreter);
			return Reflect.invokeObjectMethod( 
				obj, field, oa, interpreter, callstack, this );

		} catch ( UtilEvalError e ) {
			throw e.toEvalError( this, callstack );
		}
	}

	
	static int getIndexAux(
		Object obj, CallStack callstack, Interpreter interpreter, 
		SimpleNode callerInfo ) 
		throws EvalError
	{
		if ( !obj.getClass().isArray() )
			throw new EvalError("Not an array", callerInfo, callstack );

		int index;
		try {
			Object indexVal = 
				((SimpleNode)callerInfo.jjtGetChild(0)).eval( 
					callstack, interpreter );
			if ( !(indexVal instanceof Primitive) )
				indexVal = Types.getAssignableForm( indexVal, Integer.TYPE);
			index = ((Primitive)indexVal).intValue();
		} catch( UtilEvalError e ) {
			Interpreter.debug("doIndex: "+e);
			throw e.toEvalError( 
				"Arrays may only be indexed by integer types.", 
				callerInfo, callstack );
		}

		return index;
	}

	
	private Object doIndex( 
		Object obj, boolean toLHS, 
		CallStack callstack, Interpreter interpreter ) 
		throws EvalError, ReflectError
	{
		int index = getIndexAux( obj, callstack, interpreter, this );
		if ( toLHS )
			return new LHS(obj, index);
		else
			try {
				return Reflect.getIndex(obj, index);
			} catch ( UtilEvalError e ) {
				throw e.toEvalError( this, callstack );
			}
	}

	
	private Object doProperty( boolean toLHS,
		Object obj, CallStack callstack, Interpreter interpreter ) 
		throws EvalError
	{
		if(obj == Primitive.VOID)
			throw new EvalError( 
			"Attempt to access property on undefined variable or class name", 
				this, callstack );

		if ( obj instanceof Primitive )
			throw new EvalError("Attempt to access property on a primitive", 
				this, callstack );

		Object value = ((SimpleNode)jjtGetChild(0)).eval(
			callstack, interpreter);

		if ( !( value instanceof String ) )
			throw new EvalError(
				"Property expression must be a String or identifier.", 
				this, callstack );

		if ( toLHS )
			return new LHS(obj, (String)value);

		
		CollectionManager cm = CollectionManager.getCollectionManager();
		if ( cm.isMap( obj ) )
		{
			Object val = cm.getFromMap( obj, value );
			return ( val == null ?  val = Primitive.NULL : val );
		}

		try {
			return Reflect.getObjectProperty( obj, (String)value );
		}
		catch ( UtilEvalError e)  
		{
			throw e.toEvalError( "Property: "+value, this, callstack );
		}
		catch (ReflectError e) 
		{
			throw new EvalError("No such property: " + value, this, callstack );
		}
	}
}

