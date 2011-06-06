


package bsh;

class BSHReturnType extends SimpleNode
{
	public boolean isVoid;

	BSHReturnType(int id) { super(id); }

	public Object getReturnType( NameSpace namespace ) throws EvalError
	{
		if(isVoid)
			return Primitive.VOID;
		else
			return ((BSHType)jjtGetChild(0)).getType( namespace );
	}
}

