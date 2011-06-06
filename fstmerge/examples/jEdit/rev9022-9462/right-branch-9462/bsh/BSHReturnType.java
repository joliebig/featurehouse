


package bsh;

class BSHReturnType extends SimpleNode
{
	public boolean isVoid;

	BSHReturnType(int id) { super(id); }

	BSHType getTypeNode() { 
		return (BSHType)jjtGetChild(0);
	}

	public String getTypeDescriptor( 
		CallStack callstack, Interpreter interpreter, String defaultPackage ) 
	{
		if ( isVoid )
			return "V";
		else
			return getTypeNode().getTypeDescriptor( 
				callstack, interpreter, defaultPackage );
	}

	public Class evalReturnType( 
		CallStack callstack, Interpreter interpreter ) throws EvalError
	{
		if ( isVoid )
			return Void.TYPE;
		else
			return getTypeNode().getType( callstack, interpreter );
	}
}

