


package bsh;

class BSHArguments extends SimpleNode
{
    BSHArguments(int id) { super(id); }

    public Object[] getArguments( CallStack callstack, Interpreter interpreter)
		throws EvalError
    {
        
        Object[] args = new Object[jjtGetNumChildren()];
        for(int i = 0; i < args.length; i++)
            args[i] = ((SimpleNode)jjtGetChild(i)).eval(callstack, interpreter);

        return args;
    }
}

