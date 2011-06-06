


package bsh;

class BSHBlock extends SimpleNode
{
	BSHBlock(int id) { super(id); }

	public Object eval( CallStack callstack, Interpreter interpreter) 
		throws EvalError
	{
		return eval( callstack, interpreter, false );
	}

	
	public Object eval( 
		CallStack callstack, Interpreter interpreter, 
		boolean overrideNamespace ) 
		throws EvalError
	{
		Object ret = Primitive.VOID;
		int statements = jjtGetNumChildren();

		NameSpace enclosingNameSpace = null;
		if ( !overrideNamespace ) 
		{
			enclosingNameSpace= callstack.top();
			BlockNameSpace bodyNameSpace = 
				new BlockNameSpace( enclosingNameSpace );



			callstack.swap( bodyNameSpace );
		}

		try {
			for(int i=0; i<statements; i++)
			{
				SimpleNode node = ((SimpleNode)jjtGetChild(i));
				ret = node.eval( callstack, interpreter );

				
				if (ret instanceof ReturnControl)
					break;
			}
		} finally {
			
			if ( !overrideNamespace ) 
				callstack.swap( enclosingNameSpace );
		}

		return ret;
	}
}

