


package bsh;


class BSHFormalParameters extends SimpleNode
{
	int numArgs;






	String[] argNames;
	Class[] argTypes;

	BSHFormalParameters(int id) { super(id); }

	
	public Object eval( NameSpace namespace )  
		throws EvalError
	{
		numArgs = jjtGetNumChildren();

		argNames = new String[numArgs];
		argTypes = new Class[numArgs];

		for(int i=0; i<numArgs; i++)
		{
			BSHFormalParameter param = (BSHFormalParameter)jjtGetChild(i);
			param.eval( namespace );
			argNames[i] = param.name;
			argTypes[i] = param.type;
		}

		return Primitive.VOID;
	}
}

