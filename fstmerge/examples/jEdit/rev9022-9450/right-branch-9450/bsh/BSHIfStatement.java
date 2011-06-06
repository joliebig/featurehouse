


package bsh;

class BSHIfStatement extends SimpleNode
{
    BSHIfStatement(int id) { super(id); }

    public Object eval(CallStack callstack, Interpreter interpreter)  
		throws EvalError
    {
        Object ret = null;

        if( evaluateCondition( 
			(SimpleNode)jjtGetChild(0), callstack, interpreter ) )
            ret = ((SimpleNode)jjtGetChild(1)).eval(callstack, interpreter);
        else
            if(jjtGetNumChildren() > 2)
                ret = ((SimpleNode)jjtGetChild(2)).eval(callstack, interpreter);

        if(ret instanceof ReturnControl)
            return ret;
        else    
            return Primitive.VOID;
    }

    public static boolean evaluateCondition(
		SimpleNode condExp, CallStack callstack, Interpreter interpreter) 
		throws EvalError
    {
        Object obj = condExp.eval(callstack, interpreter);
        if(obj instanceof Primitive) {
			if ( obj == Primitive.VOID )
				throw new EvalError("Condition evaluates to void type", 
					condExp, callstack );
            obj = ((Primitive)obj).getValue();
		}

        if(obj instanceof Boolean)
            return ((Boolean)obj).booleanValue();
        else
            throw new EvalError(
				"Condition must evaluate to a Boolean or boolean.", 
				condExp, callstack );
    }
}
