


package bsh;

class BSHLHSPrimaryExpression extends SimpleNode
{
	BSHLHSPrimaryExpression(int id) { super(id); }

	public LHS toLHS(CallStack callstack, Interpreter interpreter) 
		throws EvalError
	{
		
		int childNum = 0;
		SimpleNode prefixNode = (SimpleNode)jjtGetChild(childNum++);
		Object prefixValue = null;
		LHS lhs = null;
		if ( prefixNode instanceof BSHAmbiguousName )   {
			lhs = ((BSHAmbiguousName)prefixNode).toLHS( callstack, interpreter);

		} else
			
			prefixValue = 
				((SimpleNode)prefixNode).eval( callstack, interpreter);

		
		
		
		if ( prefixValue != null )
			lhs = ((BSHLHSPrimarySuffix)jjtGetChild(childNum++)).doLHSSuffix(
				prefixValue, callstack, interpreter);

		
		int numChildren = jjtGetNumChildren(); 
		while( childNum<numChildren ) 
			lhs = ((BSHLHSPrimarySuffix)jjtGetChild(childNum++)).doLHSSuffix(
				lhs.getValue(), callstack, interpreter);

		return lhs;
	}
}

