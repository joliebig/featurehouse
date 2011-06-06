


package org.gjt.sp.jedit.bsh;

class BSHArguments extends SimpleNode
{
    BSHArguments(int id) { super(id); }

	
	
    public Object[] getArguments( CallStack callstack, Interpreter interpreter)
		throws EvalError
    {
        
        Object[] args = new Object[jjtGetNumChildren()];
        for(int i = 0; i < args.length; i++)
		{
            args[i] = ((SimpleNode)jjtGetChild(i)).eval(callstack, interpreter);
			if ( args[i] == Primitive.VOID )
				throw new EvalError( "Undefined argument: " + 
					((SimpleNode)jjtGetChild(i)).getText(), this, callstack );
		}

        return args;
    }
}

