


package bsh;

class BSHReturnStatement extends SimpleNode implements ParserConstants
{
	public int kind;

	BSHReturnStatement(int id) { super(id); }

	public Object eval(CallStack callstack, Interpreter interpreter)  
		throws EvalError
	{
		Object value;
		if(jjtGetNumChildren() > 0)
			value = ((SimpleNode)jjtGetChild(0)).eval(callstack, interpreter);
		else
			value = Primitive.VOID;

		return new ReturnControl( kind, value, this );
	}
}

