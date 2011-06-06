


package bsh;

class BSHThrowStatement extends SimpleNode
{
	BSHThrowStatement(int id) { super(id); }

	public Object eval( CallStack callstack, Interpreter interpreter)  
		throws EvalError
	{
		Object obj = ((SimpleNode)jjtGetChild(0)).eval(callstack, interpreter);

		
		
		if(!(obj instanceof Exception))
			throw new EvalError("Expression in 'throw' must be Exception type", this);

		
		throw new TargetError((Exception)obj, this);
	}
}

