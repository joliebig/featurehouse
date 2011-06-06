


package bsh;


class BSHCastExpression extends SimpleNode {

    public BSHCastExpression(int id) { super(id); }

	
	public Object eval(
		CallStack callstack, Interpreter interpreter ) throws EvalError
    {
		NameSpace namespace = callstack.top();
        Class toType = ((BSHType)jjtGetChild(0)).getType(namespace);
		SimpleNode expression = (SimpleNode)jjtGetChild(1);

        
        Object fromValue = expression.eval(callstack, interpreter);
        Class fromType = fromValue.getClass();

		try {
			return castObject( fromValue, toType );
		} catch ( EvalError e ) {
			e.reThrow( this );
			throw new InterpreterError("can't happen"); 
		}
    }

	
	public static Object castObject( Object fromValue, Class toType )
		throws EvalError
	{
        Class fromType = fromValue.getClass();

		
		
        Object result = null;

		
        if ( toType.isPrimitive() ) 
			if ( fromValue instanceof Primitive )
				result = castPrimitive( (Primitive)fromValue, toType );
			else
				
                castError(fromValue.getClass(), toType);
        else 
			
			if ( fromValue instanceof Primitive )
				
				result = castPrimitive( (Primitive)fromValue, toType );
			else
				
				
				if ( Capabilities.canGenerateInterfaces() &&
					(fromValue instanceof bsh.This) && toType.isInterface() ) 
						result = ((bsh.This)fromValue).getInterface( toType );
				else 
					
					
					if ( toType.isInstance(fromValue ) )
						result = fromValue;
					else
						castError(fromType, toType);

		if ( result == null )
			throw new InternalError("bad construct somewhere...");

		return result;
	}

	
    public static void castError(Class from, Class to) throws EvalError {
		castError( 
			Reflect.normalizeClassName(from), Reflect.normalizeClassName(to) );
    }

    public static void castError(String from, String to) throws EvalError 
	{
		Exception cce = new ClassCastException("Illegal cast. Cannot cast " +
            from + " to " + to );
		throw new TargetError( "Cast", cce );
    }

	
	public static Primitive castPrimitive( Primitive primValue, Class toType ) 
		throws EvalError
	{
		
		if ( primValue == Primitive.VOID )
			castError( "void value", Reflect.normalizeClassName(toType) );

		
		Object value = primValue.getValue();
		Class fromType = primValue.getType();

		
		
		if ( !toType.isPrimitive() )
			if ( primValue != Primitive.NULL )
				castError("primitive value", "object type:" + toType);
			else
				return primValue;

		
		if ( fromType == Boolean.TYPE )
		{
			if ( toType != Boolean.TYPE )
				castError(fromType, toType);
			else 
				return primValue;
		}

		

		
		if (value instanceof Character)
			value = new Integer(((Character)value).charValue());

		if (value instanceof Number)
		{
			Number number = (Number)value;

			if (toType == Byte.TYPE)
				value = new Primitive(number.byteValue());
			else if(toType == Short.TYPE)
				value = new Primitive(number.shortValue());
			else if(toType == Character.TYPE)
				value = new Primitive((char)number.intValue());
			else if(toType == Integer.TYPE)
				value = new Primitive(number.intValue());
			else if(toType == Long.TYPE)
				value = new Primitive(number.longValue());
			else if(toType == Float.TYPE)
				value = new Primitive(number.floatValue());
			else if(toType == Double.TYPE)
				value = new Primitive(number.doubleValue());
			else
				castError(fromType, toType);

			return (Primitive)value;
		} 

		throw new EvalError("unknown type in cast");
	}
}
