


package org.gjt.sp.jedit.bsh;

class BSHBlock extends SimpleNode
{
	public boolean isSynchronized = false;

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
		Object syncValue = null;
		if ( isSynchronized ) 
		{
			
			SimpleNode exp = ((SimpleNode)jjtGetChild(0));
			syncValue = exp.eval(callstack, interpreter);
		}

		Object ret;
		if ( isSynchronized ) 
			synchronized( syncValue )
			{
				ret = evalBlock( 
					callstack, interpreter, overrideNamespace, null);
			}
		else
				ret = evalBlock( 
					callstack, interpreter, overrideNamespace, null );

		return ret;
	}

	Object evalBlock( 
		CallStack callstack, Interpreter interpreter, 
		boolean overrideNamespace, NodeFilter nodeFilter ) 
		throws EvalError
	{	
		Object ret = Primitive.VOID;
		NameSpace enclosingNameSpace = null;
		if ( !overrideNamespace ) 
		{
			enclosingNameSpace= callstack.top();
			BlockNameSpace bodyNameSpace = 
				new BlockNameSpace( enclosingNameSpace );

			callstack.swap( bodyNameSpace );
		}

		int startChild = isSynchronized ? 1 : 0;
		int numChildren = jjtGetNumChildren();

		try {
			
			for(int i=startChild; i<numChildren; i++)
			{
				SimpleNode node = ((SimpleNode)jjtGetChild(i));

				if ( nodeFilter != null && !nodeFilter.isVisible( node ) )
					continue;

				if ( node instanceof BSHClassDeclaration )
					node.eval( callstack, interpreter );
			}
			for(int i=startChild; i<numChildren; i++)
			{
				SimpleNode node = ((SimpleNode)jjtGetChild(i));
				if ( node instanceof BSHClassDeclaration )
					continue;

				
				if ( nodeFilter != null && !nodeFilter.isVisible( node ) )
					continue;

				ret = node.eval( callstack, interpreter );

				
				if ( ret instanceof ReturnControl )
					break;
			}
		} finally {
			
			if ( !overrideNamespace ) 
				callstack.swap( enclosingNameSpace );
		}
		return ret;
	}

	public interface NodeFilter {
		public boolean isVisible( SimpleNode node );
	}

}

