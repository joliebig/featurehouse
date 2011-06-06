

package bsh;


class BSHFormalParameter extends SimpleNode
{
	public static final Class UNTYPED = null;
	public String name;
	
	public Class type;

	BSHFormalParameter(int id) { super(id); }

	public String getTypeDescriptor( 
		CallStack callstack, Interpreter interpreter, String defaultPackage ) 
	{
		if ( jjtGetNumChildren() > 0 )
			return ((BSHType)jjtGetChild(0)).getTypeDescriptor( 
				callstack, interpreter, defaultPackage );
		else
			
			return "Ljava/lang/Object;";  
	}

	
	public Object eval( CallStack callstack, Interpreter interpreter) 
		throws EvalError
	{
		if ( jjtGetNumChildren() > 0 )
			type = ((BSHType)jjtGetChild(0)).getType( callstack, interpreter );
		else
			type = UNTYPED;

		return type;
	}
}

