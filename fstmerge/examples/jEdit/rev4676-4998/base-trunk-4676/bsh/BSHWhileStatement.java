


package bsh;


class BSHWhileStatement extends SimpleNode implements ParserConstants
{
	public boolean isDoStatement;

    BSHWhileStatement(int id) { super(id); }

    public Object eval( CallStack callstack, Interpreter interpreter)  
		throws EvalError
    {
		int numChild = jjtGetNumChildren();

		
        SimpleNode condExp, body = null;

		if ( isDoStatement ) {
			condExp = (SimpleNode)jjtGetChild(1);
			body =(SimpleNode)jjtGetChild(0);
		} else {
			condExp = (SimpleNode)jjtGetChild(0);
			if ( numChild > 1 )	
				body =(SimpleNode)jjtGetChild(1);
		}

		boolean doOnceFlag = isDoStatement;
        while( 
			doOnceFlag || 
			BSHIfStatement.evaluateCondition(condExp, callstack, interpreter )
		)
		{
			if ( body == null ) 
				continue;

			Object ret = body.eval(callstack, interpreter);

			boolean breakout = false;
			if(ret instanceof ReturnControl)
			{
				switch(((ReturnControl)ret).kind )
				{
					case RETURN:
						return ret;

					case CONTINUE:
						continue;

					case BREAK:
						breakout = true;
						break;
				}
			}
			if(breakout)
				break;

			doOnceFlag = false;
		}

        return Primitive.VOID;
    }

}
