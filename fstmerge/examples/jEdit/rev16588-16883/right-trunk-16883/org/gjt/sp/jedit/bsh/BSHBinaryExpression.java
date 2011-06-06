


package org.gjt.sp.jedit.bsh;


class BSHBinaryExpression extends SimpleNode 
	implements ParserConstants 
{
    public int kind;

    BSHBinaryExpression(int id) { super(id); }

    public Object eval( CallStack callstack, Interpreter interpreter)  
		throws EvalError
    {
        Object lhs = ((SimpleNode)jjtGetChild(0)).eval(callstack, interpreter);

		
        if (kind == INSTANCEOF)
        {
			
			if ( lhs == Primitive.NULL )
				return new Primitive(false);

            Class rhs = ((BSHType)jjtGetChild(1)).getType( 
				callstack, interpreter );
		
			
			if ( lhs instanceof Primitive )
				if ( rhs == org.gjt.sp.jedit.bsh.Primitive.class )
					return new Primitive(true);
				else
					return new Primitive(false);

			
            boolean ret = Types.isJavaBaseAssignable( rhs, lhs.getClass() );
            return new Primitive(ret);
        }


		
		

		
		if ( kind == BOOL_AND || kind == BOOL_ANDX ) {
			Object obj = lhs;
			if ( isPrimitiveValue(lhs) )
				obj = ((Primitive)lhs).getValue();
			if ( obj instanceof Boolean && 
				( ((Boolean)obj).booleanValue() == false ) )
				return new Primitive(false);
		}
		
		if ( kind == BOOL_OR || kind == BOOL_ORX ) {
			Object obj = lhs;
			if ( isPrimitiveValue(lhs) )
				obj = ((Primitive)lhs).getValue();
			if ( obj instanceof Boolean && 
				( ((Boolean)obj).booleanValue() == true ) )
				return new Primitive(true);
		}

		

		
		boolean isLhsWrapper = isWrapper( lhs );
        Object rhs = ((SimpleNode)jjtGetChild(1)).eval(callstack, interpreter);
		boolean isRhsWrapper = isWrapper( rhs );
		if ( 
			( isLhsWrapper || isPrimitiveValue( lhs ) )
			&& ( isRhsWrapper || isPrimitiveValue( rhs ) )
		)
        {
			
			if ( (isLhsWrapper && isRhsWrapper && kind == EQ)) 
			{
				
			} else
				try {
					return Primitive.binaryOperation(lhs, rhs, kind);
				} catch ( UtilEvalError e ) {
					throw e.toEvalError( this, callstack  );
				}
        }
	

		
		
        switch(kind)
        {
            case EQ:
                return new Primitive((lhs == rhs));

            case NE:
                return new Primitive((lhs != rhs));

            case PLUS:
                if(lhs instanceof String || rhs instanceof String)
                    return lhs.toString() + rhs.toString();

            

            default:
                if(lhs instanceof Primitive || rhs instanceof Primitive)
                    if ( lhs == Primitive.VOID || rhs == Primitive.VOID )
                        throw new EvalError(
				"illegal use of undefined variable, class, or 'void' literal", 
							this, callstack );
                    else 
					if ( lhs == Primitive.NULL || rhs == Primitive.NULL )
                        throw new EvalError(
				"illegal use of null value or 'null' literal", this, callstack);

                throw new EvalError("Operator: '" + tokenImage[kind] +
                    "' inappropriate for objects", this, callstack );
        }
    }

	
	private boolean isPrimitiveValue( Object obj ) {
        return ( (obj instanceof Primitive) 
			&& (obj != Primitive.VOID) && (obj != Primitive.NULL) );
	}

	
	private boolean isWrapper( Object obj ) {
        return ( obj instanceof Boolean || 
			obj instanceof Character || obj instanceof Number );
	}
}
