


package bsh;

class BSHStatementExpressionList extends SimpleNode
{
	BSHStatementExpressionList(int id) { super(id); }

	public Object eval(CallStack callstack, Interpreter interpreter)  
		throws EvalError
	{
		int n = jjtGetNumChildren();
		for(int i=0; i<n; i++)
		{
			SimpleNode node = ((SimpleNode)jjtGetChild(i));
			node.eval(callstack, interpreter);
		}
		return Primitive.VOID;
	}
}

