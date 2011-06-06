

package org.gjt.sp.jedit.bsh;

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
			throw new EvalError("Empty switch statement.", this, callstack );
		label = ((BSHSwitchLabel)jjtGetChild(child++));

		
		while ( child < numchild && returnControl == null ) 
		{
			
			if ( label.isDefault 
				|| primitiveEquals( 
					switchVal, label.eval( callstack, interpreter ), 
					callstack, switchExp )
				)
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

	
	private boolean primitiveEquals( 
		Object switchVal, Object targetVal, 
		CallStack callstack, SimpleNode switchExp  ) 
		throws EvalError
	{
		if ( switchVal instanceof Primitive || targetVal instanceof Primitive )
			try {
				
				Object result = Primitive.binaryOperation( 
					switchVal, targetVal, ParserConstants.EQ );
				result = Primitive.unwrap( result ); 
				return result.equals( Boolean.TRUE ); 
			} catch ( UtilEvalError e ) {
				throw e.toEvalError(
					"Switch value: "+switchExp.getText()+": ", 
					this, callstack );
			}
		else
			return switchVal.equals( targetVal );
	}
}

