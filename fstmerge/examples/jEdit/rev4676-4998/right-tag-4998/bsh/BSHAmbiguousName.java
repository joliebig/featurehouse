


package bsh;

class BSHAmbiguousName extends SimpleNode
{
    public String text;

    BSHAmbiguousName(int id) { super(id); }
	
    public Name getName( NameSpace namespace )
    {
        return namespace.getNameResolver( text );
    }

    public Object toObject(CallStack callstack, Interpreter interpreter) 
		throws EvalError
    {
        return getName( callstack.top() ).toObject( callstack, interpreter );
    }

    public Class toClass(NameSpace namespace) throws EvalError
    {
        return getName(namespace).toClass();
    }

    public LHS toLHS( CallStack callstack, Interpreter interpreter)
		throws EvalError
    {
        return getName( callstack.top() ).toLHS( callstack, interpreter );
    }

	
    public Object eval( CallStack callstack, Interpreter interpreter ) 
		throws EvalError
    {
		throw new InterpreterError( 
			"Don't know how to eval an ambiguous name!"
			+"  Use toObject() if you want an object." );
    }

	public String toString() {
		return "AmbigousName: "+text;
	}
}

