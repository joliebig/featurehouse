


package bsh;

class BSHPrimaryExpression extends SimpleNode
{
	BSHPrimaryExpression(int id) { super(id); }

	
	public Object eval(CallStack callstack, Interpreter interpreter)  
		throws EvalError
	{
		Object obj = jjtGetChild(0);
		int n = jjtGetNumChildren(); 

		for(int i=1; i<n; i++)
			obj = ((BSHPrimarySuffix)jjtGetChild(i)).doSuffix(
				obj, callstack, interpreter);

		
		if ( obj instanceof SimpleNode )
			if ( obj instanceof BSHAmbiguousName )
				obj = ((BSHAmbiguousName)obj).toObject(callstack, interpreter);
			else
				obj = ((SimpleNode)obj).eval(callstack, interpreter);	

		return obj;
	}
}

