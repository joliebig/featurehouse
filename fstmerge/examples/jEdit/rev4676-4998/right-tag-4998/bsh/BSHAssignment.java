


package bsh;

class BSHAssignment extends SimpleNode implements ParserConstants
{
    public int operator;

    BSHAssignment(int id) { super(id); }

    public Object eval(
		CallStack callstack, Interpreter interpreter) 
		throws EvalError
    {
        BSHLHSPrimaryExpression lhsNode = 
			(BSHLHSPrimaryExpression)jjtGetChild(0);

if ( lhsNode == null )
	throw new InterpreterError( "Error, null LHSnode" );

        LHS lhs = lhsNode.toLHS( callstack, interpreter);
        if ( lhs == null )
            throw new InterpreterError( "Error, null LHS" );

        Object rhs = ((SimpleNode)jjtGetChild(1)).eval(callstack, interpreter);
        if ( rhs == Primitive.VOID )
            throw new EvalError("Void assignment.", this);

        switch(operator)
        {
            case ASSIGN:
				try {
					return lhs.assign(rhs);
				} catch ( EvalError e ) {
					e.reThrow(this);
				}

            case PLUSASSIGN:
                return lhs.assign(operation(lhs.getValue(), rhs, PLUS));

            case MINUSASSIGN:
                return lhs.assign(operation(lhs.getValue(), rhs, MINUS));

            case STARASSIGN:
                return lhs.assign(operation(lhs.getValue(), rhs, STAR));

            case SLASHASSIGN:
                return lhs.assign(operation(lhs.getValue(), rhs, SLASH));

            case ANDASSIGN:
            case ANDASSIGNX:
                return lhs.assign(operation(lhs.getValue(), rhs, BIT_AND));

            case ORASSIGN:
            case ORASSIGNX:
                return lhs.assign(operation(lhs.getValue(), rhs, BIT_OR));

            case XORASSIGN:
                return lhs.assign(operation(lhs.getValue(), rhs, XOR));

            case MODASSIGN:
                return lhs.assign(operation(lhs.getValue(), rhs, MOD));

            case LSHIFTASSIGN:
            case LSHIFTASSIGNX:
                return lhs.assign(operation(lhs.getValue(), rhs, LSHIFT));

            case RSIGNEDSHIFTASSIGN:
            case RSIGNEDSHIFTASSIGNX:
                return lhs.assign(operation(lhs.getValue(), rhs, RSIGNEDSHIFT));

            case RUNSIGNEDSHIFTASSIGN:
            case RUNSIGNEDSHIFTASSIGNX:
                return lhs.assign(operation(lhs.getValue(), rhs, RUNSIGNEDSHIFT));

            default:
                throw new InterpreterError("unimplemented operator in assignment BSH");
        }
    }

    private Object operation(Object lhs, Object rhs, int kind) 
		throws EvalError
    {
		
		if ( lhs instanceof String && rhs != Primitive.VOID ) {
			if ( kind != PLUS )
				throw new EvalError(
					"Use of non + operator with String LHS", this);     

			return (String)lhs + rhs;
		}

        if ( lhs instanceof Primitive || rhs instanceof Primitive )
            if(lhs == Primitive.VOID || rhs == Primitive.VOID)
                throw new EvalError(
					"Illegal use of undefined object or 'void' literal", this);
            else if ( lhs == Primitive.NULL || rhs == Primitive.NULL )
                throw new EvalError(
					"Illegal use of null object or 'null' literal", this);


        if( (lhs instanceof Boolean || lhs instanceof Character ||
             lhs instanceof Number || lhs instanceof Primitive) &&
            (rhs instanceof Boolean || rhs instanceof Character ||
             rhs instanceof Number || rhs instanceof Primitive) )
        {
            return Primitive.binaryOperation(lhs, rhs, kind);
        }

        throw new EvalError("Non primitive value in operator: " +
            lhs.getClass() + " " + tokenImage[kind] + " " + rhs.getClass(), this);
    }
}
