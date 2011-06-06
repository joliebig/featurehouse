


package bsh;

class BSHAssignment extends SimpleNode implements ParserConstants
{
    public int operator;

    BSHAssignment(int id) { super(id); }

    public Object eval(
		CallStack callstack, Interpreter interpreter) 
		throws EvalError
    {
        BSHPrimaryExpression lhsNode = 
			(BSHPrimaryExpression)jjtGetChild(0);

		if ( lhsNode == null )
			throw new InterpreterError( "Error, null LHSnode" );

		boolean strictJava = interpreter.getStrictJava();
        LHS lhs = lhsNode.toLHS( callstack, interpreter);
        if ( lhs == null )
            throw new InterpreterError( "Error, null LHS" );

		
		
		
		Object lhsValue = null;
		if ( operator != ASSIGN ) 
			try {
				lhsValue = lhs.getValue();
			} catch ( UtilEvalError e ) {
				throw e.toEvalError( this, callstack );
			}

        SimpleNode rhsNode = (SimpleNode)jjtGetChild(1);

        Object rhs;
		
		
		
		
		
        rhs = rhsNode.eval(callstack, interpreter);

        if ( rhs == Primitive.VOID )
            throw new EvalError("Void assignment.", this, callstack );

		try {
			switch(operator)
			{
				case ASSIGN:
					return lhs.assign( rhs, strictJava );

				case PLUSASSIGN:
					return lhs.assign( 
						operation(lhsValue, rhs, PLUS), strictJava );

	            case MINUSASSIGN:
					return lhs.assign( 
						operation(lhsValue, rhs, MINUS), strictJava );

				case STARASSIGN:
					return lhs.assign( 
						operation(lhsValue, rhs, STAR), strictJava );

	            case SLASHASSIGN:
					return lhs.assign( 
						operation(lhsValue, rhs, SLASH), strictJava );

	            case ANDASSIGN:
				case ANDASSIGNX:
					return lhs.assign( 
						operation(lhsValue, rhs, BIT_AND), strictJava );

	            case ORASSIGN:
	            case ORASSIGNX:
	                return lhs.assign( 
						operation(lhsValue, rhs, BIT_OR), strictJava );

	            case XORASSIGN:
	                return lhs.assign( 
						operation(lhsValue, rhs, XOR), strictJava );

	            case MODASSIGN:
	                return lhs.assign( 
						operation(lhsValue, rhs, MOD), strictJava );

	            case LSHIFTASSIGN:
	            case LSHIFTASSIGNX:
	                return lhs.assign( 
						operation(lhsValue, rhs, LSHIFT), strictJava );

	            case RSIGNEDSHIFTASSIGN:
	            case RSIGNEDSHIFTASSIGNX:
	                return lhs.assign( 
					operation(lhsValue, rhs, RSIGNEDSHIFT ), strictJava );

	            case RUNSIGNEDSHIFTASSIGN:
	            case RUNSIGNEDSHIFTASSIGNX:
	                return lhs.assign( 
						operation(lhsValue, rhs, RUNSIGNEDSHIFT), 
						strictJava );

				default:
					throw new InterpreterError(
						"unimplemented operator in assignment BSH");
			}
		} catch ( UtilEvalError e ) {
			throw e.toEvalError( this, callstack );
		}
    }

    private Object operation( Object lhs, Object rhs, int kind ) 
		throws UtilEvalError
    {
		
		if ( lhs instanceof String && rhs != Primitive.VOID ) {
			if ( kind != PLUS )
				throw new UtilEvalError(
					"Use of non + operator with String LHS" );

			return (String)lhs + rhs;
		}

        if ( lhs instanceof Primitive || rhs instanceof Primitive )
            if(lhs == Primitive.VOID || rhs == Primitive.VOID)
                throw new UtilEvalError(
					"Illegal use of undefined object or 'void' literal" );
            else if ( lhs == Primitive.NULL || rhs == Primitive.NULL )
                throw new UtilEvalError(
					"Illegal use of null object or 'null' literal" );


        if( (lhs instanceof Boolean || lhs instanceof Character ||
             lhs instanceof Number || lhs instanceof Primitive) &&
            (rhs instanceof Boolean || rhs instanceof Character ||
             rhs instanceof Number || rhs instanceof Primitive) )
        {
            return Primitive.binaryOperation(lhs, rhs, kind);
        }

        throw new UtilEvalError("Non primitive value in operator: " +
            lhs.getClass() + " " + tokenImage[kind] + " " + rhs.getClass() );
    }
}
