


package bsh;


class BSHFormalParameter extends SimpleNode
{
	public static final Class UNTYPED = null;
	public String name;
	public Class type;

	BSHFormalParameter(int id) { super(id); }

	
	public Object eval( NameSpace namespace )  
		throws EvalError
	{
		if(jjtGetNumChildren() > 0)
			type = ((BSHType)jjtGetChild(0)).getType(namespace);
		else
			type = UNTYPED;

		return Primitive.VOID;
	}
}

