

package bsh;


class Types 
{
	
	static final int CAST=0, ASSIGNMENT=1;
	
	static final int 
		JAVA_BASE_ASSIGNABLE = 1,
		JAVA_BOX_TYPES_ASSIGABLE = 2,
		JAVA_VARARGS_ASSIGNABLE = 3,
		BSH_ASSIGNABLE = 4;

	static final int
		FIRST_ROUND_ASSIGNABLE = JAVA_BASE_ASSIGNABLE,
		LAST_ROUND_ASSIGNABLE = BSH_ASSIGNABLE;

	
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

	
	
	static boolean isSignatureAssignable( Class[] from, Class[] to, int round )
	{
		if ( round != JAVA_VARARGS_ASSIGNABLE && from.length != to.length )
			return false;

		switch ( round )
		{
			case JAVA_BASE_ASSIGNABLE:
				for( int i=0; i<from.length; i++ )
					if ( !isJavaBaseAssignable( to[i], from[i] ) )
						return false;
				return true;
			case JAVA_BOX_TYPES_ASSIGABLE:
				for( int i=0; i<from.length; i++ )
					if ( !isJavaBoxTypesAssignable( to[i], from[i] ) )
						return false;
				return true;
			case JAVA_VARARGS_ASSIGNABLE:
				return isSignatureVarargsAssignable( from, to );
			case BSH_ASSIGNABLE:
				for( int i=0; i<from.length; i++ )
					if ( !isBshAssignable( to[i], from[i] ) )
						return false;
				return true;
			default:
				throw new InterpreterError("bad case");
		}
	}

	private static boolean isSignatureVarargsAssignable(
		Class[] from, Class[] to )
	{
		return false;
	}

	
	static boolean isJavaAssignable( Class lhsType, Class rhsType ) {
		return isJavaBaseAssignable( lhsType, rhsType )
			|| isJavaBoxTypesAssignable( lhsType, rhsType );
	}

	
	static boolean isJavaBaseAssignable( Class lhsType, Class rhsType )
	{
		
		if ( lhsType == null )
			return false;

		
		
		if ( rhsType == null )
			return !lhsType.isPrimitive();

		if ( lhsType.isPrimitive() && rhsType.isPrimitive() )
		{
			if ( lhsType == rhsType )
				return true;

			
			if ( (rhsType == Byte.TYPE) &&
				(lhsType == Short.TYPE || lhsType == Integer.TYPE
				|| lhsType == Long.TYPE || lhsType == Float.TYPE
				|| lhsType == Double.TYPE))
                    return true;

            if ( (rhsType == Short.TYPE) &&
				(lhsType == Integer.TYPE || lhsType == Long.TYPE ||
                lhsType == Float.TYPE || lhsType == Double.TYPE))
                    return true;

            if ((rhsType == Character.TYPE) &&
				(lhsType == Integer.TYPE || lhsType == Long.TYPE ||
                lhsType == Float.TYPE || lhsType == Double.TYPE))
                    return true;

            if ((rhsType == Integer.TYPE) &&
				(lhsType == Long.TYPE || lhsType == Float.TYPE ||
                lhsType == Double.TYPE))
                    return true;

            if ((rhsType == Long.TYPE) &&
				(lhsType == Float.TYPE || lhsType == Double.TYPE))
                return true;

            if ((rhsType == Float.TYPE) && (lhsType == Double.TYPE))
                return true;
        }
        else
            if ( lhsType.isAssignableFrom(rhsType) )
                return true;

        return false;
    }

	
	static boolean isJavaBoxTypesAssignable(
		Class lhsType, Class rhsType )
	{
		
		if ( lhsType == null )
			return false;

		
		if ( lhsType == Object.class )
			return true;

		
		if ( lhsType == Number.class
			&& rhsType != Character.TYPE
			&& rhsType != Boolean.TYPE
		)
			return true;

		
		
		
		
		if ( Primitive.wrapperMap.get( lhsType ) == rhsType )
			return true;

		return false;
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

	
	
	private static Object castObject(
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
