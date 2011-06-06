

package bsh;


class Types 
{
	static final int CAST=0, ASSIGNMENT=1;
	
	static Primitive VALID_CAST = new Primitive(1);
	static Primitive INVALID_CAST = new Primitive(-1);
	
    public static Class[] getTypes( Object[] args )
    {
        if ( args == null )
            return new Class[0];

        Class[] types = new Class[ args.length ];

        for( int i=0; i<args.length; i++ )
        {
			if ( args[i] == null )
				types[i] = null;
            else 
			if ( args[i] instanceof Primitive )
                types[i] = ((Primitive)args[i]).getType();
            else
                types[i] = args[i].getClass();
        }

        return types;
    }

	
	static boolean argsAssignable( Class [] parameters, Object [] args )
	{
		Class [] argTypes = getTypes( args );
		return isSignatureAssignable( argTypes, parameters );
	}

	
	
    static boolean isSignatureAssignable( Class[] from, Class[] to )
    {
        if ( from.length != to.length )
            return false;

        for(int i=0; i<from.length; i++)
            if ( !isBshAssignable( to[i], from[i] ) )
                return false;

        return true;
    }

    
    static boolean isJavaAssignable( Class lhs, Class rhs )
    {
		
		
		if ( rhs == null ) 
			return !lhs.isPrimitive();

		if ( lhs.isPrimitive() && rhs.isPrimitive() )
		{
			if ( lhs == rhs )
				return true;

			
			if ( (rhs == Byte.TYPE) && 
				(lhs == Short.TYPE || lhs == Integer.TYPE ||
                lhs == Long.TYPE || lhs == Float.TYPE || lhs == Double.TYPE))
                    return true;

            if ( (rhs == Short.TYPE) && 
				(lhs == Integer.TYPE || lhs == Long.TYPE ||
                lhs == Float.TYPE || lhs == Double.TYPE))
                    return true;

            if ((rhs == Character.TYPE) && 
				(lhs == Integer.TYPE || lhs == Long.TYPE ||
                lhs == Float.TYPE || lhs == Double.TYPE))
                    return true;

            if ((rhs == Integer.TYPE) && 
				(lhs == Long.TYPE || lhs == Float.TYPE ||
                lhs == Double.TYPE))
                    return true;

            if ((rhs == Long.TYPE) && 
				(lhs == Float.TYPE || lhs == Double.TYPE))
                return true;

            if ((rhs == Float.TYPE) && (lhs == Double.TYPE))
                return true;
        }
        else
            if ( lhs.isAssignableFrom(rhs) )
                return true;

        return false;
    }

	
    public static Object getAssignableForm( Object rhs, Class lhsType )
		throws UtilEvalError
    {
		return castObject( rhs, lhsType, ASSIGNMENT );
    }

	
	public static Object castObject( 
		Object fromValue, Class toType, int operation ) 
		throws UtilEvalError
	{
		if ( fromValue == null )
			throw new InterpreterError("null fromValue");

		Class fromType = 
			fromValue instanceof Primitive ? 
				((Primitive)fromValue).getType() 
				: fromValue.getClass();

		return castObject( 
			toType, fromType, fromValue, operation, false );
	}

	static boolean isBshAssignable( Class toType, Class fromType )
	{
		try {
			return castObject( 
				toType, fromType, null, 
				ASSIGNMENT, true 
			) == VALID_CAST;
		} catch ( UtilEvalError e ) {
			
			throw new InterpreterError("err in cast check: "+e);
		}
	}

	
	static Object castObject( 
		Class toType, Class fromType, Object fromValue, 
		int operation, boolean checkOnly )
		throws UtilEvalError
	{
		
		if ( checkOnly && fromValue != null )
			throw new InterpreterError("bad cast params 1");
		if ( !checkOnly && fromValue == null )
			throw new InterpreterError("bad cast params 2");
		if ( fromType == Primitive.class )
			throw new InterpreterError("bad from Type, need to unwrap");
		if ( fromValue == Primitive.NULL && fromType != null )
			throw new InterpreterError("inconsistent args 1");
		if ( fromValue == Primitive.VOID && fromType != Void.TYPE )
			throw new InterpreterError("inconsistent args 2");
		if ( toType == Void.TYPE )
			throw new InterpreterError("loose toType should be null");
		
		
		if ( toType == null || toType == fromType )
			return checkOnly ? VALID_CAST :
				fromValue;

        if ( toType.isPrimitive() ) 
		{
			if ( fromType == Void.TYPE || fromType == null 
				|| fromType.isPrimitive() )
			{
				
				return Primitive.castPrimitive( 
					toType, fromType, (Primitive)fromValue, 
					checkOnly, operation );
			} else
			{
				if ( Primitive.isWrapperType( fromType ) )
				{
					
					

					
					Class unboxedFromType = Primitive.unboxType( fromType );
					Primitive primFromValue;
					if ( checkOnly ) 
						primFromValue = null; 
					else
						primFromValue = (Primitive)Primitive.wrap( 
							fromValue, unboxedFromType );

					return Primitive.castPrimitive( 
						toType, unboxedFromType, primFromValue, 
						checkOnly, operation );
				} else
				{
					
					if ( checkOnly )
						return INVALID_CAST;
					else
						throw castError( toType, fromType, operation );
				}
			}
        }

		
		if ( fromType == Void.TYPE || fromType == null
			|| fromType.isPrimitive() )
		{
			if ( Primitive.isWrapperType( toType ) 
				&& fromType != Void.TYPE && fromType != null )
			{
				
				return checkOnly ? VALID_CAST :
					Primitive.castWrapper( 
						Primitive.unboxType(toType), 
						((Primitive)fromValue).getValue() );
			}

			
			if ( toType == Object.class 
				&& fromType != Void.TYPE && fromType != null )
			{
				
				return checkOnly ? VALID_CAST :
					((Primitive)fromValue).getValue();
			}

			
			
			
			return Primitive.castPrimitive( 
				toType, fromType, (Primitive)fromValue, checkOnly, operation );
		}

		
		
		
		if ( toType.isAssignableFrom( fromType ) )
			return checkOnly ? VALID_CAST : 
				fromValue;

		
		
		if ( toType.isInterface() 
			&& bsh.This.class.isAssignableFrom( fromType ) 
			&& Capabilities.canGenerateInterfaces() 
		)
			return checkOnly ? VALID_CAST : 
				((bsh.This)fromValue).getInterface( toType );

		
		
		if ( Primitive.isWrapperType( toType ) 
			&& Primitive.isWrapperType( fromType ) 
		)
			return checkOnly ? VALID_CAST :
				Primitive.castWrapper( toType, fromValue );
		
		if ( checkOnly )
			return INVALID_CAST;
		else
			throw castError( toType, fromType , operation  );
	}

	
    static UtilEvalError castError( 
		Class lhsType, Class rhsType, int operation   ) 
    {
		return castError( 
			Reflect.normalizeClassName(lhsType),
			Reflect.normalizeClassName(rhsType), operation  );
    }

    static UtilEvalError castError( 
		String lhs, String rhs, int operation   ) 
    {
		if ( operation == ASSIGNMENT )
			return new UtilEvalError (
				"Can't assign " + rhs + " to "+ lhs );

		Exception cce = new ClassCastException(
			"Cannot cast " + rhs + " to " + lhs );
		return new UtilTargetError( cce );
    }

	
	

	
}
