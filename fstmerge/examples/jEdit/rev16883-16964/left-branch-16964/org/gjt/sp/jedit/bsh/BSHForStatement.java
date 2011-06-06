


package org.gjt.sp.jedit.bsh;


class BSHForStatement extends SimpleNode implements ParserConstants
{
    public boolean hasForInit;
    public boolean hasExpression;
    public boolean hasForUpdate;

    private SimpleNode forInit;
    private SimpleNode expression;
    private SimpleNode forUpdate;
    private SimpleNode statement;

    private boolean parsed;

    BSHForStatement(int id) { super(id); }

    public Object eval(CallStack callstack , Interpreter interpreter)  
		throws EvalError
    {
        int i = 0;
        if(hasForInit)
            forInit = ((SimpleNode)jjtGetChild(i++));
        if(hasExpression)
            expression = ((SimpleNode)jjtGetChild(i++));
        if(hasForUpdate)
            forUpdate = ((SimpleNode)jjtGetChild(i++));
        if(i < jjtGetNumChildren()) 
            statement = ((SimpleNode)jjtGetChild(i));

		NameSpace enclosingNameSpace= callstack.top();
		BlockNameSpace forNameSpace = new BlockNameSpace( enclosingNameSpace );

		

		
		
		
		callstack.swap( forNameSpace );

        
        if ( hasForInit ) 
            forInit.eval( callstack, interpreter );

		Object returnControl = Primitive.VOID;
        while(true)
        {
            if ( hasExpression ) 
			{
				boolean cond = BSHIfStatement.evaluateCondition(
					expression, callstack, interpreter );

				if ( !cond ) 
					break;
			}

            boolean breakout = false; 
            if ( statement != null ) 
            {
				
                Object ret = statement.eval( callstack, interpreter );

                if (ret instanceof ReturnControl)
                {
                    switch(((ReturnControl)ret).kind)
                    {
                        case RETURN:
							returnControl = ret;
							breakout = true;
                            break;

                        case CONTINUE:
                            break;

                        case BREAK:
                            breakout = true;
                            break;
                    }
                }
            }

            if ( breakout )
                break;

            if ( hasForUpdate )
                forUpdate.eval( callstack, interpreter );
        }

		callstack.swap( enclosingNameSpace );  
        return returnControl;
    }

}
