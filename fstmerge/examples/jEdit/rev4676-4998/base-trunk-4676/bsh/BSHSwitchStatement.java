

package bsh;

class BSHSwitchStatement 
	extends SimpleNode 
	implements ParserConstants 
{

	public BSHSwitchStatement(int id) { super(id); }

    public Object eval( CallStack callstack, Interpreter interpreter )
		throws EvalError
	{
		int numchild = jjtGetNumChildren();
		int child = 0;
		SimpleNode switchExp = ((SimpleNode)jjtGetChild(child++));
		Object switchVal = switchExp.eval( callstack, interpreter );

		
		
		BSHSwitchLabel label;
		Object node;
		ReturnControl returnControl=null;

		
		if ( child >= numchild )
			throw new EvalError("Empty switch statement...");
		label = ((BSHSwitchLabel)jjtGetChild(child++));

		
		while ( child < numchild && returnControl == null ) 
		{
			
			if ( label.isDefault 
				|| label.eval( callstack, interpreter ).equals( switchVal ) )
			{
				
				while ( child < numchild ) 
				{
					node = jjtGetChild(child++);
					if ( node instanceof BSHSwitchLabel )
						continue;
					
					Object value = 
						((SimpleNode)node).eval( callstack, interpreter ); 

					
					if ( value instanceof ReturnControl ) {
						returnControl = (ReturnControl)value;
						break;
					}
				}
			} else 
			{
				
				while ( child < numchild ) 
				{
					node = jjtGetChild(child++);
					if ( node instanceof BSHSwitchLabel ) {
						label = (BSHSwitchLabel)node;
						break;
					}
				}
			}
		}

		if ( returnControl != null && returnControl.kind == RETURN )
			return returnControl;
		else
			return Primitive.VOID;
	}

}

